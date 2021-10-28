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

-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API

-define(ERLANG_PREDS,
  [
    {{register_gproc, 1}, ?MODULE, register_gproc_predicate},
    {{join_cc, 2}, ?MODULE, join_cc_predicate},
    {{cc, 4}, ?MODULE, cc_predicate},
    {{send, 2}, ?MODULE, gproc_send_predicate}
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
  case lists:any(fun(El) -> case El of  {_} -> true; _ -> false end end, ListD) of
    true ->
      %% All parameters need to be binded
      erlog_int:fail(St);
    _ ->
      gproc:reg({p, g, DCCId}, DOntology),
      erlog_int:prove_body(Next0, St)
  end.

%%to_predicate({_, CCId, Agent, Parent}, Next0, #est{bs = Bs} = St) ->
%%  ?INFO_MSG("--->Looking for TO on CC :~p",[{CCId, {Agent, Parent}}]),
%%  [DCCId, DAgent, DParent] = DOptionList = erlog_int:dderef([CCId, Agent, Parent], Bs),
%%  case erlog:vars_in(DOptionList) of
%%    [_] -> %At least one parameter is binded
%%      GProcKey = {p, g, to_query_param(DCCId)},
%%      MatchHead = {GProcKey, '_', {to_query_param(DAgent), to_query_param(DParent)}},
%%      Guard = [],
%%      Result = ['$$'],
%%      SelectedCCs = gproc:select([{MatchHead, Guard, Result}]),
%%      ?INFO_MSG("Selected CCs :~p",[SelectedCCs]),
%%      cc_predicate2(DCCId, DAgent, DParent, Next0, St, SelectedCCs);
%%    _ ->
%%      erlog_int:fail(St)
%%  end.


cc_predicate({_, CCId, Ontology}, Next0, #est{bs = Bs} = St) ->
  ?INFO_MSG("--->Looking for CC :~p",[{CCId, Ontology}]),
  [DCCId, DOntology] = erlog_int:dderef([CCId, Ontology], Bs),
  GProcKey = {p, g, to_query_param(DCCId)},
  MatchHead = {GProcKey, '_', to_query_param(DOntology)},
  Guard = [],
  Result = ['$$'],
  SelectedCCs = gproc:select([{MatchHead, Guard, Result}]),
  ?INFO_MSG("Selected CCs :~p",[SelectedCCs]),

  cc_predicate2(DCCId, DOntology, Next0, St, SelectedCCs).

cc_predicate2(_DCCId, _DOntology, _Next0, St, []) ->
  erlog_int:fail(St);

cc_predicate2(DCCId, DOntology, Next0, #est{bs = Bs, vn = Vn, cps = Cps} = St,
    [[{_, _, RCCID}, _, ROntology] | Tail]) ->
  ?INFO_MSG("cc_predicate2",[]),

  case unify_cc([{DCCId, RCCID}, {DOntology, ROntology}], Bs) of
    {succeed, NewBs} ->
      ?INFO_MSG("Unified",[]),

      FailFun =
        fun(#cp{next=NextF,bs=Bs0,vn=Vnf}, LCps, Lst) ->
          cc_predicate2(DCCId, DOntology, NextF, Lst#est{cps = LCps, bs = Bs0, vn = Vnf + 4}, Tail)
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
        ?WARNING_MSG("CMEEUUHUUH :~p", [CcId]),

  case  erlog_int:dderef([Message, CcId], Bs) of
    [{_} = CcId, _] ->
      ?WARNING_MSG("Call to send with unbinded CC :~p", [CcId]),
      %% Message must be binded
      erlog_int:fail(St);
    [DMessage, DCcId] when is_binary(DCcId) ->
      ?DEBUG("Sending ~p on CC ~p", [DMessage, DCcId]),
      gproc:send({p, g, DCcId}, {incoming_gproc, DCcId, DMessage}),
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