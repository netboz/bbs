%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 07. Jan 2021 8:29 PM
%%%-------------------------------------------------------------------

-module(ont_bbs_bubble).

-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

-export([external_predicates/0]).
-export([
    register_bubble_predicate/3,
    spawn_child_predicate/3,
    stop_child_predicate/3,
    terminate_child_predicate/3
]).

%% Prolog API
-define(ERLANG_PREDS, [
    {{register_bubble, 1}, ?MODULE, register_bubble_predicate},
    {{spawn_child, 2}, ?MODULE, spawn_child_predicate},
    {{stop_child, 1}, ?MODULE, stop_child_predicate},
    {{terminate_child, 1}, ?MODULE, terminate_child_predicate}
]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Return the list of built in predicates contained in this module
%%
%% @end
%%------------------------------------------------------------------------------

external_predicates() ->
    ?ERLANG_PREDS.

register_bubble_predicate({_Atom, NodeName}, Next0, #est{bs = Bs} = St) ->
    DNodeName =
        case erlog_int:dderef(NodeName, Bs) of
            {_} ->
                uuid:get_v4_urandom();
            _ ->
                NodeName
        end,

    case bbs_agents_backend:register_bubble(get(agent_name), DNodeName) of
        ok ->
            erlog_int:unify_prove_body(DNodeName, NodeName, Next0, St);
        {error, Reason} ->
            ?ERROR_MSG("Failled to register bubble :~p", [Reason]),
            erlog_int:fail(St)
    end.

%% @doc spawn a child locally on the AP. AgSpecs contains Agent descripion
spawn_child_predicate({_Atom, {agent, Name, Onts}, Ref}, Next0, #est{bs = Bs} = St) ->
    case erlog_int:dderef(Ref, Bs) of
        {_} ->
            ChildNode = get(children_node),
            ParentName = get(agent_name),
            AgentSpecs =
                #agent{
                    name = Name,
                    parent = ParentName,
                    node = ChildNode,
                    startup_ontologies = Onts
                },

            case bbs_agents_backend:spawn_agent(AgentSpecs) of
                {ok, AgentRef} ->
                    ?INFO_MSG("Spawned agent: ~p", [{Name, ChildNode, ParentName}]),
                    erlog_int:unify_prove_body(Ref, AgentRef, Next0, St);
                {error, Reason} ->
                    ?ERROR_MSG("Failled to create agent :~p Reason :~p", [Name, Reason]),
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end.

%% @doc Gracefully ask a local child to stop.
stop_child_predicate({_Atom, AgentRef}, Next0, #est{bs = Bs} = St) ->
    DRef = erlog_int:dderef(AgentRef, Bs),
    ?INFO_MSG("Stopping ~p", [DRef]),

    case bbs_agents_backend:stop_agent(DRef) of
        ok ->
            erlog_int:prove_body(Next0, St);
        {error, Reason} ->
            ?ERROR_MSG("Failled to stop child : ~p    Reason: ~p", [
                DRef, Reason
            ]),
            erlog_int:fail(St)
    end.

%% @doc Brutally terminate child process
terminate_child_predicate({_Atom, AgentRef}, Next0, #est{bs = Bs} = St) ->
    DRef = erlog_int:dderef(AgentRef, Bs),
    case bbs_agents_backend:terminate_agent(DRef) of
        ok ->
            erlog_int:prove_body(Next0, St);
        {error, Reason} ->
            ?ERROR_MSG("Failled to terminate child : ~p    Reason: ~p", [
                DRef, Reason
            ]),
            erlog_int:fail(St)
    end.

%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%
