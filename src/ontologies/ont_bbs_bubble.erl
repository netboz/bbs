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
-export([spawn_child/3, stop_child/3, terminate_child/3, is_pid_alive/3]).

%% Prolog API
-define(ERLANG_PREDS,
  [
    {{spawn_child, 2}, ?MODULE, spawn_child},
    {{stop_child, 1}, ?MODULE, stop_child},
    {{terminate_child, 1}, ?MODULE, terminate_child},
    {{is_pid_alive, 1}, ?MODULE, is_pid_alive}
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
spawn_child({_Atom, {agent, Name, Onts}, PidBack}, Next0, #est{} = St) ->
  case bbs_agent:start_link(#agent{name = Name,startup_ontologies = Onts}) of
    {ok, Pid} ->
      erlog_int:unify_prove_body(PidBack, Pid, Next0, St);
    {error, Reason} ->
      ?ERROR_MSG("Failled to create agent :~p Reason :~p", [Name, Reason]),
      erlog_int:fail(St)
  end.

%% @doc Grafelly ask a local child to stop.
stop_child({_Atom, Pid}, Next0, #est{} = St) ->
  case erlang:is_process_alive(Pid) of
    true ->
      ?DEBUG("before exit", []),

      exit(Pid, normal),
      ?DEBUG("after exit", []),

      erlog_int:prove_body(Next0, St);
    _ ->
      erlog_int:fail(St)
  end.

%% @doc Brutally terminate child process
terminate_child({_Atom, Pid}, Next0, #est{} = St) ->
  case erlang:is_process_alive(Pid) of
    true ->
      exit(Pid,kill),
      erlog_int:prove_body(Next0, St);
    _ ->
      erlog_int:fail(St)
  end.

%% @doc Checks if current pid is running
is_pid_alive({_Atom, Pid}, Next0, #est{} = St) ->
  case erlang:is_process_alive(Pid) of
    true ->
      erlog_int:prove_body(Next0, St);
    _ ->
      erlog_int:fail(St)
  end.