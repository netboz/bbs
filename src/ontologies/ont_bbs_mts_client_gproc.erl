%%%-------------------------------------------------------------------
%%% @author netboz
%%% @copyright (C) 2015, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 15. May. 2021 23:23
%%%-------------------------------------------------------------------
-module('ont_bbs_mts_client_gproc').

-author("netboz").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API

-define(ERLANG_PREDS,
  [
    {{register_gproc, 1}, ?MODULE, register_gproc_predicate},
    {{join_cc, 2}, ?MODULE, join_cc_predicate},
    {{cc, 4}, ?MODULE, cc_predicate},
    {{gproc_send, 2}, ?MODULE, gproc_send_predicate}
  ]).

%% API
-export([external_predicates/0]).
-export([register_gproc_predicate/3, join_cc_predicate/3, cc_predicate/3, gproc_send_predicate/3]).

external_predicates() ->
  ?ERLANG_PREDS.

register_gproc_predicate({_, ParentBubble}, Next0, #est{bs = Bs} = St) ->
  ?INFO_MSG("Registering gproc",[]),
  case erlog_int:dderef(ParentBubble, Bs) of
    {_} ->
      erlog_int:fail(St);
    _ ->
      MyName = get(agent_name),
      case gproc:reg({n, g, {MyName, ParentBubble}}) of
        {gproc_error, Reason} ->
          ?INFO_MSG("Could not register agent ~p to gproc, reason :~p", [{MyName, ParentBubble}, Reason]),
          erlog_int:fail(St);
        true ->
          erlog_int:prove_body(Next0, St)
      end
  end.


join_cc_predicate({_, CCId, Ontology}, Next0, #est{bs = Bs} = St) ->
  ?INFO_MSG("Joining cc ~p  ~p  ", [CCId, Ontology]),
  [DCCId, DOntology] = ListD = erlog_int:dderef([CCId, Ontology], Bs),
  Parent = get(parent),
  case lists:any(fun(El) -> case El of  {_} -> true; _ -> false end end, ListD) of
    true ->
      erlog_int:fail(St);
    _ ->
      Me = get(agent_name),
      gproc:reg({p, g, DCCId}, {Me, Parent, DOntology} ),
      erlog_int:prove_body(Next0, St)
  end.

cc_predicate({_, CCId, To, Ontology, Parent}, Next0, #est{bs = Bs} = St) ->
  ?INFO_MSG("--->Looking for CC :~p",[{CCId, To, Ontology, Parent}]),
  [DCCId, DTo, DOntology, DParent] = erlog_int:dderef([CCId, To, Ontology, Parent], Bs),
  GProcKey = {p, g, to_query_param(DCCId)},
  MatchHead = {GProcKey, '_', {to_query_param(DTo), to_query_param(DParent), to_query_param(DOntology)}},
  Guard = [],
  Result = ['$$'],
  SelectedCCs = gproc:select([{MatchHead, Guard, Result}]),
  ?INFO_MSG("Selected CCs :~p",[SelectedCCs]),

  cc_predicate2(DCCId, DTo, DOntology, DParent, Next0, St, SelectedCCs).

cc_predicate2(_DCCId, _DTo, _DOntology, _DParent, _Next0, St, []) ->
  erlog_int:fail(St);

cc_predicate2(DCCId, DTo, DOntology, DParent, Next0, #est{bs = Bs, vn = Vn, cps = Cps} = St,
    [[{_, _, RCCID}, _, {RTo, RParent, ROntology}] | Tail]) ->
  ?INFO_MSG("cc_predicate2",[]),

  case unify_cc([{DCCId, RCCID}, {DTo, RTo}, {DParent, RParent}, {DOntology, ROntology}], Bs) of
    {succeed, NewBs} ->
      ?INFO_MSG("Unified",[]),

      FailFun =
        fun(#cp{next=NextF,bs=Bs0,vn=Vnf}, LCps, Lst) ->
          cc_predicate2(DCCId, DTo, DOntology, DParent, NextF, Lst#est{cps = LCps, bs = Bs0, vn = Vnf + 4}, Tail)
        end,
      Cp = #cp{type = compiled,
        data = FailFun,
        next = Next0,
        bs = Bs,
        vn = Vn},
      erlog_int:prove_body(Next0, St#est{bs = NewBs, cps = [Cp|Cps]});
    _ ->
      ?INFO_MSG("NOT unified",[]),
      erlog_int:fail(St)
  end.

gproc_send_predicate({_, CcId, Message}, Next0, #est{bs = Bs} = St) ->
  AgentName = get(agent_name),
  Parent = get(parent),
  case  erlog_int:dderef([Message, CcId], Bs) of
    [DMessage, DCcId] when is_binary(DMessage) andalso is_binary(DCcId) ->
      gproc:send({p, g, DCcId}, {gproc, AgentName, Parent, DCcId, DMessage}),
      erlog_int:prove_body(Next0, St);
    _ ->
      erlog_int:fail(St)
  end.

unify_cc([], Bs) ->
  {succeed, Bs};
unify_cc([{D,R}|Tail], Bs) ->
  case erlog_int:unify(D,R, Bs) of
    {succeed, NewBs} ->
      unify_cc(Tail, NewBs);
    fail ->
      fail
  end.

to_query_param({_}) ->
  '_';
to_query_param(Value) ->
  Value.