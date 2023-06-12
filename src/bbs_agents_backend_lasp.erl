%%%-----------------------------------------------------------------------------
%%% @doc
%%% Registry for BBS based on Lasp
%%% @author yan
%%% @end
%%%-----------------------------------------------------------------------------

-module(bbs_agents_backend_lasp).
-author("yan").

-behavior(bbs_agents_backend).

-include("bbs.hrl").
-include("utils.hrl").

-export([init/1, register_bubble/2, spawn_agent/1, stop_agent/1, terminate_agent/1, timestamp/0]).

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================
-spec init(_) -> 'ok'.

init(_) ->
    AwMapType = {state_awmap, [state_mvregister]},

    % declare the variable
    {ok, {_MapAgentToBubbleNode, _, _, _}} = lasp:declare(
        {<<"agent_to_bubble_node">>, AwMapType}, AwMapType
    ),
    {ok, {_MapBubbleNodeToAgent, _, _, _}} = lasp:declare(
        {<<"bubble_node_to_agent">>, AwMapType}, AwMapType
    ),
    ok.

register_bubble(Agent, Node) ->
    % Update lasp registry
    lasp:update(
        {<<"agent_to_bubble_node">>, {state_awmap, [state_mvregister]}},
        {apply, Agent, {set, timestamp(), Node}},
        [self()]
    ),
    lasp:update(
        {<<"bubble_node_to_agent">>, {state_awmap, [state_mvregister]}},
        {apply, Node, {set, timestamp(), Agent}},
        [self()]
    ),
    ok.

spawn_agent(#agent{name = Name, parent = Parent, node = Node, startup_ontologies = StOnts}) ->
    G = erleans:get_grain(bbs_statefull_agent_grain, {Name, Node, Parent}),
    ?DEBUG("Grain ~p", [G]),

    bbs_statefull_agent_grain:new_ontology(G, StOnts),
    Pid = erleans_pm:whereis_name(G),
    erlang:monitor(process, Pid, [{tag, {'agent_down', Name}}]),
    lasp_pg:join(Node, Pid, false),
    ?DEBUG("PID ~p", [Pid]),
    {ok, G}.

stop_agent(AgentRef) ->
    bbs_statefull_agent_grain:stop(AgentRef).

terminate_agent(AgentRef) ->
    case erleans_pm:whereis_name(AgentRef) of
        undefined ->
            ?WARNING_MSG("Process not found ~p", [AgentRef]),
            {error, grain_not_found};
        Pid ->
            exit(Pid, kill),
            ok
    end.

timestamp() ->
    erlang:unique_integer([monotonic, positive]).
