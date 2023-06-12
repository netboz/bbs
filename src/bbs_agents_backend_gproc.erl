%%%-----------------------------------------------------------------------------
%%% @doc
%%% This agent backend consder an agent is an ontology before being a process.
%%% 
%%% @author yan
%%% @end
%%%-----------------------------------------------------------------------------

-module(bbs_agents_backend_gproc).
-author("yan").

-behavior(bbs_agents_backend).

-include("bbs.hrl").

-export([init/1, register_bubble/2, spawn_agent/1, stop_agent/1, terminate_agent/1]).

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================

init(_) ->
    ok.

register_bubble(Agent, Node) ->
    gproc:reg({n, l, {agent_to_bubble_node, Agent}}, Node),
    gproc:reg({n, l, {bubble_node_to_agent, Node}}, Agent),
    ok.

spawn_agent(#agent{id = Id, node = Node, parent = , startup_ontologies = StartupOntologies}) ->
    {ok, ok}.

stop_agent(AgentRef) ->
    gproc:unreg({n, l, {agent_to_bubble_node, AgentRef}}),
    gproc:unreg({n, l, {bubble_node_to_agent, AgentRef}}),
    ok.

terminate_agent(AgentRef) ->
    gproc:unreg({n, l, {agent_to_bubble_node, AgentRef}}),
    gproc:unreg({n, l, {bubble_node_to_agent, AgentRef}}),
    ok.



