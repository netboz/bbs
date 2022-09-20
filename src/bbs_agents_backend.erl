%%%-----------------------------------------------------------------------------
%%% @doc
%%% Header built from template
%%% @author yan
%%% @end
%%%-----------------------------------------------------------------------------

-module(bbs_agents_backend).
-author("yan").

-export([init/1, register_bubble/2, spawn_agent/1, stop_agent/1, terminate_agent/1]).

%%%=============================================================================
%%% Global Definitions
%%%=============================================================================

-callback init(Args :: term()) ->
    ok | {error, Reason :: term()}.

-callback register_bubble(AgentName :: binary(), NodeName :: binary()) ->
    ok | {error, Reason :: term()}.

-callback spawn_agent(AgentSpecs :: term()) ->
    {ok, AgentRef :: term()} | {error, Reason :: binary()}.

-callback stop_agent(AgentRef :: term()) ->
    ok | {error, Reason :: binary()}.

-callback terminate_agent(AgentRef :: term()) ->
    ok | {error, Reason :: binary()}.

init(Args) ->
    BackendMod = persistent_term:get(agent_backend_mod),
    BackendMod:init(Args).

register_bubble(AgentName, NodeName) ->
    BackendMod = persistent_term:get(agent_backend_mod),
    BackendMod:register_bubble(AgentName, NodeName).

spawn_agent(AgentSpecs) ->
    BackendMod = persistent_term:get(agent_backend_mod),
    BackendMod:spawn_agent(AgentSpecs).

stop_agent(AgentRef) ->
    BackendMod = persistent_term:get(agent_backend_mod),
    BackendMod:stop_agent(AgentRef).

terminate_agent(AgentRef) ->
    BackendMod = persistent_term:get(agent_backend_mod),
    BackendMod:terminate_agent(AgentRef).
