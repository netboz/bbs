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

%% API
-export([spawn_agent/3, terminate_agent/3]).

spawn_agent({_Atom, Supervisor, #agent{} = AgSpecs, PidBack}, Next0, #est{} = St) ->
  case supervisor:start_child(Supervisor, [AgSpecs]) of
    {ok, Pid} ->
      erlog_int:unify_prove_body(PidBack, Pid, Next0, St);
    {error, Reason} ->
      ?ERROR_MSG("Failled to create agent :~p Reason :~p", [AgSpecs#agent.name, Reason]),
      erlog_int:fail(St)
  end.



terminate_agent({_Atom, AgentN, Reason}, Next0, #est{} = St) ->
  Pid = gproc:lookup_pid({n, l, {AgentN, node()}}),
  gen_fsm:send_all_state_event(Pid, {stop_from_ams, Reason}),
  erlog_int:prove_body(Next0, St).
