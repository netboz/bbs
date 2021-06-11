%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc FSM describing a bbs agent
%%%
%%% @end
%%% Created : 10. Jan 2020 23:22
%%%-------------------------------------------------------------------
-module(bbs_agent).

-author("Yan").

-behaviour(gen_statem).

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

%% API
-export([start_link/1]).
%% gen_statem States
-export([ontologies_init/3, running/3]).
%% gen_statem other callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
%% Used to retrieve and store initial state for an ontology
-export([get_ontology_state_from_namespace/1, store_ontology_state_on_namespace/2]).

-define(SERVER, ?MODULE).

-record(state, {uuid, parent, onto_to_init = [], onto_dict}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(#agent{} = AgentSpecs) ->
    gen_statem:start(?MODULE, AgentSpecs, []).

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(AgentSpecs) ->
    ?INFO_MSG("Initialising Agent :~p   PID :~p", [AgentSpecs, self()]),
    %% Right now we are trapping exit to manage child processes, even if we don't have one
    process_flag(trap_exit, true),

    %% Create agent Identifier
    AgentName =
        case AgentSpecs#agent.name of
            undefined ->
                zuuid:binary(
                    zuuid:v4());
            SpecificName when is_binary(SpecificName) ->
                SpecificName;
            _InvalidName ->
                ?ERROR_MSG("Invalid name specified :~p ", [_InvalidName]),
                zuuid:binary(
                    zuuid:v4())
        end,
    %% We store agent name in process dictionary for easy retrieval during predicate proving process
    put(agent_name, AgentName),
    put(parent, AgentSpecs#agent.parent),

    %% Finish init and start ontologies initialisation
    {ok,
        ontologies_init,
        #state{onto_dict = dict:new(), uuid = AgentName, parent = AgentSpecs#agent.parent},
        [{next_event, cast, {init_next, AgentSpecs#agent.startup_ontologies}}]}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    [state_functions, state_enter].

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% First state after init, we starts by initialising the ontologies of the agent
%% @end
%%------------------------------------------------------------------------------

ontologies_init(enter, _, State) ->
    ?INFO_MSG("Ontology init :~p.", [State#state.uuid]),
    {keep_state, State};
ontologies_init(cast, {init_next, []}, State) ->
    %% Call the run predicate in each initialised ontologys
    ?INFO_MSG("All Ontologies initialised.", []),
    {next_state, running, State};
ontologies_init(cast,
    {init_next, [{ontology, Ns, Params, DbMod} | NextOntos]},
    #state{} = State) ->
    case bbs_ontology:build_ontology(State#state.uuid, Ns, DbMod) of
        {error, OntoBuildError} ->
            ?ERROR_MSG("~p Failled to initialize ontology :~p with error: ~p",
                [State#state.uuid, Ns, OntoBuildError]),
            {stop, {ontologies_creation_failled, Ns, OntoBuildError}, State};
        {ok, {_Tid, Kb}} ->
            ?INFO_MSG("Builded Knowledge Base for :~p", [Ns]),
            %% Set the prolog engine to not crash when a predicate isn't found. TODO : Guess this should be done earlier
            {succeed, KbReady} = erlog_int:prove_goal({set_prolog_flag, unknown, fail}, Kb),

            case erlog_int:prove_goal({goal, {initialized, State#state.uuid, State#state.parent, Ns, Params}}, KbReady)
            of
                {succeed, _InitializedOntoState} ->
                    %% Register the raw kb state into process stack under key Namespace
                    undefined = store_ontology_state_on_namespace(Ns, KbReady),
                    {next_state,
                        ontologies_init,
                        State,
                        [{next_event, cast, {init_next, NextOntos}}]};
                {fail, FStatr} ->
                    ?ERROR_MSG("Ontology initialisation failed :~p with state: ~p", [Ns, FStatr]),
                    {stop, {ontologies_initialisation_failed, Ns}, State};
                Else ->
                    ?ERROR_MSG("Ontology unexpected result :~p", [Else]),
                    {stop, {ontologies_initialisation_failed, Ns}, State}
            end
    end;

ontologies_init(info, {'EXIT', Pid, Reason}, State) ->
    ?INFO_MSG("Child ~p EXIT with reason :~p",
        [State#state.uuid, Pid, Reason]),
    {stop, Reason, State};


%% Ideally this message received from a monitor should be managed by next clause, but because message from monitor
%% doesn't start from an atom, it crash the prolog engine...hence this dedicated clause.
ontologies_init(info, {{'DOWN', Name}, _ref, process, Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p EXITED with reason :~p",
        [State#state.uuid, Pid, Reason]),
    _ = trigger_agent_reactions(info_event, {child_down, Name, Reason}),
    {next_state, ontologies_init, State};

ontologies_init(Type, Message, State) when is_tuple(Message) ->
    ?INFO_MSG("~p Got event :~p   ~p", [State#state.uuid, {Type, Message}, self()]),
    %% The purpose of this is only to avoid using the event Type directly into prolog, because 'call' type
    %% collides with the one in ontologies, included by erlog_int
    case Type of
        call ->
            _ = trigger_agent_reactions(sync_event, Message);
        cast ->
            _ = trigger_agent_reactions(async_event, Message);
        info ->
            _ = trigger_agent_reactions(info_event, Message)
    end,
    {next_state, ontologies_init, State};

ontologies_init(UnkMesTyp, UnkMes, State) ->
    ?INFO_MSG("Agent ~p Received unmanaged state messsage :~p",
        [State#state.uuid, {UnkMesTyp, UnkMes}]),
    {next_state, running, State}.
%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Ontologies are initialized, we now sequentially call ```boot```
%% predicate on all of them
%% @end
%%------------------------------------------------------------------------------

running(enter, _, State) ->
    ?INFO_MSG("Agent ~p is running.", [State#state.uuid]),
    {keep_state, State};
%%running(cast, {NameSpace, Predicate}, State) ->
%%    ?INFO_MSG("Agent ~p checking for stims on :~p", [State#state.uuid, {NameSpace, Predicate}]),
%%    trigger_stims(NameSpace, Predicate),
%%    {next_state, running, State};

running(info, {'EXIT', Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p disconnected with reason :~p",
        [State#state.uuid, Pid, Reason]),
    {stop, Reason, State};

%% Ideally this message received from a monitor should be managed by next clause, but because message from monitor
%% doesn't start from an atom, it crash the prolog engine...hence this dedicated clause.
running(info, {{'DOWN', Name}, _ref, process, Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p EXITED with reason :~p",
        [State#state.uuid, Pid, Reason]),
    _ = trigger_agent_reactions(info_event, {child_down, Name, Reason}),
    {next_state, running, State};

running(Type, Message, State) when is_tuple(Message)->
    ?INFO_MSG("~p Got event :~p   ~p", [State#state.uuid, {Type, Message}, self()]),

    %% The purpose of this is only to avoid using the event Type directly into prolog, because 'call' type
    %% collides with the one in ontologies, included by erlog_int
    case Type of
        call ->
            _ = trigger_agent_reactions(sync_event, Message);
        cast ->
            _ = trigger_agent_reactions(async_event, Message);
        info ->
            _ = trigger_agent_reactions(info_event, Message)
    end,
    {next_state, running, State};

running(UnkMesTyp, UnkMes, State) ->
    ?INFO_MSG("Agent ~p Received unmanaged state messsage :~p",
        [State#state.uuid, {UnkMesTyp, UnkMes}]),
    {next_state, running, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, State = #state{}) ->
    ?INFO_MSG("Agent :~p   PID :~p terminating", [State#state.uuid, self()]),
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #state{}, _Extra) ->
    {ok, StateName, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

prove(NameSpace, Predicate) ->
    %TODO: next instruction fail, should mimic the failling of a prolog predicate
    bbs_ontology:prove(NameSpace, Predicate).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%%  Get an raw ontology state stored under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec get_ontology_state_from_namespace(Ns :: binary()) -> undefined | tuple().
get_ontology_state_from_namespace(Ns) ->
    get(Ns).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Store an ontology state under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec store_ontology_state_on_namespace(Ns :: binary(), Kb :: term()) ->
    undefined | term().
store_ontology_state_on_namespace(Ns, KbDb) ->
    put(Ns, KbDb).

%%-------------------------------------------------------------------------------
%% @doc
%% @private
%%  check all hooks stored for Namespace:Predicate and run the ones whose
%%  parameters can be unified.
%% @end
%%------------------------------------------------------------------------------

trigger_agent_reactions(Type, Message) ->
    prove(<<"bbs:agent">>, {goal, {agent_event_processed, Type, Message}}).
