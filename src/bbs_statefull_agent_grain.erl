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

-export([placement/0, provider/0, save/1, new_event_sync/2, new_event_async/2, stop/1]).
-export([activate/2, handle_call/3, handle_cast/2, handle_info/2, deactivate/1]).

%-include_lib("erleans/src/erleans.hrl").

placement() ->
    prefer_local.

provider() ->
    default.

activate(X, #agent{name = undefined} = Agent) ->
    activate(X, Agent#agent{name = uuid:get_v4_urandom()});
activate(X,
         #agent{name = AgentName,
                parent = Parent,
                tree_node = TreeNode} =
             Agent) ->
    ?INFO_MSG("Activating agent grain :~p   PID :~p TreeNode :~p", [Agent, self(), TreeNode]),

    io:format("Ref is    ~p ", [X]),
    %% and parent should be managed only with bbs:agent ontology, and internally like this in process dict, but to avoid
    %% querying bbs:agent each time we want to access these values, for perf, it is here for now
    put(agent_name, AgentName),
    put(parent, Parent),
    put(tree_node, TreeNode),
    {ok,
     #agent_state{onto_dict = dict:new(),
                  uuid = AgentName,
                  tree_node = TreeNode,
                  parent = Agent#agent.parent},
     #{}}.

         
-spec new_event_sync(Ref :: erleans:grain_ref(), Event :: term()) -> Reply :: term().
new_event_sync(Ref, Event) ->
    erleans_grain:call(Ref, {new_event, Event}).

new_event_async(Ref, Event) ->
    erleans_grain:cast(Ref, {new_event, Event}).

save(Ref) ->
    erleans_grain:call(Ref, save).

stop(Ref) ->
    erleans_grain:call(Ref, stop).

handle_call(save, _From, State) ->
    {ok, State}.

handle_cast(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

deactivate(State) ->
    {ok, State}.
