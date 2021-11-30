%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, qengho
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

%% API
-export([start_link/2]).
%% gen_statem States
-export([ontologies_init/3, running/3]).
%% gen_statem other callbacks
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
%% Used to retrieve and store initial state for an ontology
-export([get_ontology_state_from_namespace/1, store_ontology_state_on_namespace/2]).

-record(agent_state, {uuid, parent, tree_node, onto_to_init = [], onto_dict}).

%%%===================================================================
%%% gen_statem callbacks

%%%===================================================================

%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    [state_functions, state_enter].

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(#agent{} = AgentSpecs, TreeNode) ->
     gen_statem:start({via, ?HORDEREG, {?BBS_BUBBLES_REG, {reg, TreeNode, AgentSpecs#agent.name}}}, 
        ?MODULE, AgentSpecs#agent{tree_node = TreeNode}, []).

%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(#agent{name = undefined} = Agent) ->
    init(Agent#agent{name = uuid:get_v4_urandom()});
init(#agent{name = AgentName, parent = Parent, tree_node = TreeNode} = AgentSpecs) when is_binary(AgentName) ->
    ?INFO_MSG("Initialising Agent :~p   PID :~p TreeNode :~p", [AgentSpecs, self(), TreeNode]),
    %% Right now we are trapping exit to manage child processes, even if we don't have one
    process_flag(trap_exit, true),

    %% Initialise randomness for this process
    quickrand:seed(),

    %% We store agent name in process dictionary for easy retrieval during predicate proving process. Ideally, agent name,
    %% and parent should be managed only with bbs:agent ontology, and internally like this in process dict, but to avoid
    %% querying bbs:agent each time we want to access these values, for perf, it is here for now
    put(agent_name, AgentName),
    put(parent, Parent),
    put(tree_node, TreeNode),

    %% For log convenience
    lager:md([{agent_name, AgentName}]),

    %% Finish init to start ontologies initialisation
    {ok,
     ontologies_init,
     #agent_state{onto_dict = dict:new(),
                  uuid = AgentName,
                  tree_node = TreeNode,
                  parent = AgentSpecs#agent.parent},
     [{next_event, cast, {init_next, AgentSpecs#agent.startup_ontologies}}]}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% First state after init, we starts by initialising the ontologies of the agent
%% @end
%%------------------------------------------------------------------------------

ontologies_init(enter, _, State) ->
    ?INFO_MSG("Ontology init :~p.", [State#agent_state.uuid]),
    {keep_state, State};
ontologies_init(cast, {init_next, []}, State) ->
    %% Call the run predicate in each initialised ontologys
    ?INFO_MSG("All Ontologies initialised.", []),
    {next_state, running, State};
ontologies_init(cast,
                {init_next, [{ontology, Ns, Params, DbMod} | NextOntos]},
                #agent_state{} = State) ->
    case bbs_ontology:build_ontology(State#agent_state.uuid, Ns, DbMod) of
        {error, OntoBuildError} ->
            ?ERROR_MSG("~p Failled to initialize ontology :~p with error: ~p",
                       [State#agent_state.uuid, Ns, OntoBuildError]),
            {stop, {ontologies_creation_failled, Ns, OntoBuildError}, State};
        {ok, {_Tid, Kb}} ->
            ?INFO_MSG("Builded Knowledge Base for :~p", [Ns]),
            %% Set the prolog engine to not crash when a predicate isn't found. TODO : Guess this should be done earlier
            {succeed, KbReady} = erlog_int:prove_goal({set_prolog_flag, unknown, fail}, Kb),

            %% Register the raw kb state into process stack under key Namespace, we need to do it even if the
            %% ontology isn't yet initialised, for hooks to be able to find the ontology.
            %% TODO : This ^^ needs to be refactored
            undefined = store_ontology_state_on_namespace(Ns, KbReady),
            case erlog_int:prove_goal({goal,
                                       {initialized,
                                        State#agent_state.uuid,
                                        State#agent_state.parent,
                                        State#agent_state.tree_node,
                                        Params}},
                                      KbReady)
            of
                {succeed, _InitializedOntoState} ->
                    erlog_int:prove_goal({goal, {tested, Ns}}, KbReady),
                    {next_state,
                     ontologies_init,
                     State,
                     [{next_event, cast, {init_next, NextOntos}}]};
                {fail, FStatr} ->
                    _ = erase(Ns),
                    ?ERROR_MSG("Ontology initialisation failed :~p with state: ~p", [Ns, FStatr]),
                    {stop, {ontologies_initialisation_failed, Ns}, State};
                Else ->
                    _ = erase(Ns),
                    ?ERROR_MSG("Ontology unexpected result :~p", [Else]),
                    {stop, {ontologies_initialisation_failed, Ns}, State}
            end
    end;
ontologies_init(info, {'EXIT', Pid, Reason}, State) ->
    ?INFO_MSG("Child ~p EXIT with reason :~p", [State#agent_state.uuid, Pid, Reason]),
    {stop, Reason, State};
%% Ideally this message received from a monitor should be managed by next clause, but because message from monitor
%% doesn't start from an atom, it crash the prolog engine...hence this dedicated clause.
ontologies_init(info, {{'DOWN', Name}, _ref, process, Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p EXITED with reason :~p",
              [State#agent_state.uuid, Pid, Reason]),
    _ = trigger_agent_reactions(info_event, {child_down, Name, Reason}),
    keep_state_and_data;
ontologies_init(Type, Message, State) when is_tuple(Message) ->
    ?INFO_MSG("~p Got event :~p   ~p", [State#agent_state.uuid, {Type, Message}, self()]),
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
    keep_state_and_data;
ontologies_init(UnkMesTyp, UnkMes, State) ->
    ?INFO_MSG("Agent ~p Received unmanaged state messsage :~p in state init",
              [State#agent_state.uuid, {UnkMesTyp, UnkMes}]),
    {next_state, running, State}.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Ontologies are initialized, we now sequentially call ```boot```
%% predicate on all of them
%% @end
%%------------------------------------------------------------------------------

running(enter, _, #agent_state{uuid = Uuid, parent = Parent} = State) ->
    ?INFO_MSG("=======> Agent ~p is running.", [Uuid]),
    %% Notify monitors with a predicate that this agent  is running
    {monitored_by, Monitors} = erlang:process_info(self(), monitored_by),
    lists:foreach(fun(Pid) -> Pid ! {running, Uuid, Parent} end, Monitors),
    _ = trigger_agent_reactions("bbs:agent", {running, Uuid, Parent}),
    {keep_state, State};
running(info, {'EXIT', Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p disconnected with reason :~p",
              [State#agent_state.uuid, Pid, Reason]),
    {stop, Reason, State};
%% Ideally this message received from a monitor should be managed by next clause, but because message from monitor
%% doesn't start from an atom, it crash the prolog engine...hence this dedicated clause.
running(info, {{'DOWN', Name}, _ref, process, Pid, Reason}, State) ->
    ?INFO_MSG("Agent ~p : Child ~p EXITED with reason :~p",
              [State#agent_state.uuid, Pid, Reason]),
    _ = trigger_agent_reactions(info_event, {child_down, Name, Reason}),
    {next_state, running, State};
running(Type, Message, State) when is_tuple(Message) ->
    ?INFO_MSG("~p Got event :~p   ~p", [State#agent_state.uuid, {Type, Message}, self()]),

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
    ?INFO_MSG("Agent ~p Received unmanaged state messsage :~p in state running",
              [State#agent_state.uuid, {UnkMesTyp, UnkMes}]),
    {next_state, running, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, State) ->
    ?INFO_MSG("Agent :~p   PID :~p terminating", [State#agent_state.uuid, self()]),
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #agent_state{}, _Extra) ->
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



