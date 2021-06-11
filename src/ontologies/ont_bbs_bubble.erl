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
-export([spawn_child_predicate/3, stop_child_predicate/3, terminate_child_predicate/3]).

-define(SWARM_REG, 'Elixir.Swarm.Registry').

%% Prolog API
-define(ERLANG_PREDS,
    [{{spawn_child, 1}, ?MODULE, spawn_child_predicate},
        {{stop_child, 1}, ?MODULE, stop_child_predicate},
        {{terminate_child, 1}, ?MODULE, terminate_child_predicate},
        {{process, 2}, ?MODULE, process_predicate}
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

%% @doc spawn a child locally on the AP. AgSpecs contains Agent descripion
spawn_child_predicate({_Atom, {agent, Name, Onts}}, Next0, #est{} = St) ->
    Me = get(agent_name),
    AgentSpecs = #agent{name = Name, parent = Me, startup_ontologies = Onts},
    case swarm:register_name(Name, bbs_agent, start_link,
        [AgentSpecs]) of
        {ok, Pid} ->
            swarm:join({parent, Me}, Pid),
            erlang:monitor(process, Pid, [{tag, {'DOWN', Name}}]),
            erlog_int:prove_body(Next0, St);
        {error, Reason} ->
            ?ERROR_MSG("Failled to create agent :~p Reason :~p", [Name, Reason]),
            erlog_int:fail(St)
    end.

%% @doc Gracefully ask a local child to stop.
stop_child_predicate({_Atom, Name}, Next0, #est{bs = Bs} = St) ->
    DName = erlog_int:dderef(Name, Bs),
    ?INFO_MSG("DNAME ~p",[DName]),

    Pid = swarm:whereis_name(DName),
    ?INFO_MSG("PID ~p",[Pid]),
    swarm:unregister_name(DName),
    case erlang:is_process_alive(Pid) of
        true ->
            exit(Pid, normal),
            ?INFO_MSG("sent exit ~p",[Pid]),
            erlog_int:prove_body(Next0, St);
        _ ->
            erlog_int:fail(St)
    end.

%% @doc Brutally terminate child process
terminate_child_predicate({_Atom, Name}, Next0, #est{bs = Bs} = St) ->
    DName = erlog_int:dderef(Name, Bs),
    Pid = swarm:whereis_name(DName),
    case erlang:is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            erlog_int:prove_body(Next0, St);
        _ ->
            erlog_int:fail(St)
    end.
