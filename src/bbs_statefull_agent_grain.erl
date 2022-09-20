%%%-----------------------------------------------------------------------------
%%% @doc
%%% Grain behavior used by erleans
%%% @author yan
%%% @end
%%%-----------------------------------------------------------------------------

-module(bbs_statefull_agent_grain).

-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================

%% example record
-record(agent_state, {uuid, parent, tree_node, onto_dict}).

-export([
    placement/0,
    provider/0,
    state/1,
    node/1,
    save/1,
    new_event_sync/2,
    new_event_async/2,
    stop/1,
    call/1
]).
-export([activate/2, handle_call/3, handle_cast/2, handle_info/2, deactivate/1]).

-export([new_ontology/2]).
-export([get_ontology_state_from_namespace/1, store_ontology_state_on_namespace/2]).
%-include_lib("erleans/src/erleans.hrl").

placement() ->
    prefer_local.

provider() ->
    default.

node(Ref) ->
    erleans_grain:call(Ref, node).

state({AgentName, Node, Parent} = X) ->
    ?INFO_MSG("Creating state ~p", [X]),
    #agent{
        name = AgentName,
        parent = Parent,
        node = Node
    }.

activate(
    X,
    #agent{
        name = AgentName,
        parent = Parent,
        node = TreeNode
    } =
        Agent
) ->
    ?INFO_MSG("Activating agent grain :~p   PID :~p TreeNode :~p", [Agent, self(), TreeNode]),

    ?INFO_MSG("Ref is    ~p ", [X]),
    %% and parent should be managed only with bbs:agent ontology, and internally like
    %% this in process dict, but to avoid querying bbs:agent each time we want to access
    %% these values, for perf, it is here for now
    put(agent_name, AgentName),
    put(parent, Parent),
    put(tree_node, TreeNode),
    {ok,
        #agent_state{
            onto_dict = dict:new(),
            uuid = AgentName,
            tree_node = TreeNode,
            parent = Agent#agent.parent
        },
        #{}}.

new_ontology(_Ref, []) ->
    ok;
new_ontology(Ref, [Onto | OtherOntos]) ->
    new_ontology(Ref, Onto),
    new_ontology(Ref, OtherOntos);
new_ontology(Ref, #ontology{} = Ontology) ->
    erleans_grain:call(Ref, {new_ontology, Ontology}).

new_event_sync(Ref, Event) ->
    erleans_grain:call(Ref, {new_event, Event}).
new_event_async(Ref, Event) ->
    erleans_grain:cast(Ref, {new_event, Event}).

save(Ref) ->
    erleans_grain:call(Ref, save).

stop(Ref) ->
    erleans_grain:call(Ref, stop).

call(Ref) ->
    erleans_grain:call(Ref, meuh).

handle_call(
    {new_ontology, #ontology{namespace = Ns, parameters = Params, storage = DbMod}}, From, State
) ->
    case bbs_ontology:build_ontology(State#agent_state.uuid, Ns, DbMod) of
        {error, OntoBuildError} ->
            ?ERROR_MSG(
                "~p Failled to initialize ontology :~p with error: ~p",
                [State#agent_state.uuid, Ns, OntoBuildError]
            ),
            ko;
        {ok, {_Tid, Kb}} ->
            ?INFO_MSG("Builded Knowledge Base for :~p", [Ns]),
            %% Set the prolog engine to not crash when a predicate isn't found.
            %% TODO : Guess this should be done earlier
            {succeed, KbReady} = erlog_int:prove_goal({set_prolog_flag, unknown, fail}, Kb),

            %% Register the raw kb state into process stack under key Namespace, we need
            %% to do it even if the ontology isn't yet initialised, for hooks to be able
            %% to find the ontology.
            %% TODO : This ^^ needs to be refactored
            undefined = store_ontology_state_on_namespace(Ns, KbReady),
            case
                erlog_int:prove_goal(
                    {goal,
                        {initialized, State#agent_state.uuid, State#agent_state.parent,
                            State#agent_state.tree_node, Params}},
                    KbReady
                )
            of
                {succeed, _InitializedOntoState} ->
                    ?INFO_MSG("Ontology initialised :~p", [Ns]),
                    %erlog_int:prove_goal({goal, {tested, Ns}}, KbReady),
                    ok;
                {fail, FStatr} ->
                    _ = erase(Ns),
                    ?ERROR_MSG("Ontology initialisation failed :~p with state: ~p", [Ns, FStatr]),
                    ko;
                Else ->
                    _ = erase(Ns),
                    ?ERROR_MSG("Ontology unexpected result :~p", [Else]),
                    ko
            end
    end,
    {ok, State, [{reply, From, ok}]};
handle_call(save, _From, State) ->
    {ok, State};
handle_call(stop, From, State) ->
    ?INFO_MSG("Deactivating :~p", [{State#agent_state.uuid, State#agent_state.parent}]),
    {deactivate, State, [{reply, From, ok}]};
handle_call(_, From, State) ->
    ?INFO_MSG("======>State is :~p", [State]),
    {ok, State, [{reply, From, ok}]}.

handle_cast(_, State) ->
    ?INFO_MSG("=======>State is :~p", [State]),
    {ok, State}.

handle_info({{agent_down, Name}, _Ref, process, _, Reason}, State) ->
    _ = trigger_agent_reactions(info_event, {child_down, Name, Reason}),
    {ok, State};
handle_info(Info, State) ->
    ?INFO_MSG("=======>Received Info :~p", [Info]),
    {ok, State}.

-spec deactivate(_) -> {'ok', _}.
deactivate(State) ->
    {ok, State}.

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

get_ontology_state_from_namespace(Ns) ->
    get(Ns).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Store an ontology state under a Namespace
%% @end
%%------------------------------------------------------------------------------
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
