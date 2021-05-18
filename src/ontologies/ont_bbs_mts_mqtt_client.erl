%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2021 10:16 PM
%%%-------------------------------------------------------------------
-module(ont_bbs_mts_mqtt_client).
-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").
-include_lib("emqx/include/emqx.hrl").
%% Prolog API

-define(ERLANG_PREDS, [
  {{new_cc, 2}, ?MODULE, new_cc_predicate},
  {{mqtt_send, 4}, ?MODULE, mqtt_send_predicate}
]).

%% API
-export([external_predicates/0]).
-export([new_cc_predicate/3, mqtt_send_predicate/3]).

external_predicates() ->
  ?ERLANG_PREDS.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Subscribe Agent to topic AgenName/AccName/Ontology<
%% If CcName is not binded, it will be binded to a uuid4
%% @end
%%------------------------------------------------------------------------------


new_cc_predicate({_, CcName, Ontology}, Next0, #est{bs = Bs} = St) ->
  BCcName = erlog_int:dderef(CcName, Bs),
  COnt = erlog_int:dderef(Ontology, Bs),
  process_new_cc(BCcName, COnt, Next0, St).

process_new_cc({_} = VarCc, Bontology, Next0, St) ->
  Me = get(agent_name),
  CCName = zuuid:v4(),
  case Bontology of
    OntName when is_binary(OntName) ->
      emqx:subscribe(iolist_to_binary([Me, <<"/">>, CCName, <<"/">>, OntName])),
      erlog_int:unify_prove_body(VarCc, CCName, Next0, St);
    _ ->
      erlog_int:fail(St)
  end;

process_new_cc(VarCc, Bontology, Next0, St) when is_binary(VarCc)->
  Me = get(agent_name),
  CCName = VarCc,
  case Bontology of
    OntName when is_binary(OntName) ->
      emqx:subscribe(iolist_to_binary([Me, <<"/">>, CCName, <<"/">>, OntName])),
      erlog_int:unify_prove_body(VarCc, CCName, Next0, St);
    _ ->
      erlog_int:fail(St)
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Send provided mqtt message on provided topic
%%
%% @end
%%------------------------------------------------------------------------------

mqtt_send_predicate({_, CcId, To, Onto, Predicate}, Next0, #est{bs = Bs} = St) ->
  DCCiD = erlog_int:dderef(CcId, Bs),
  DTo = erlog_int:dderef(To, Bs),
  DOnto = erlog_int:dderef(Onto, Bs),
  DPred = erlog_int:dderef(Predicate, Bs),
  case do_mqtt_send(DCCiD, DTo, DOnto, DPred) of
    ok -> erlog_int:prove_body(Next0, St);
    _ -> erlog_int:fail(St)
  end.

do_mqtt_send(Topic, To, Onto, Predicate) ->
  Me = get(agent_name),
  emqx:publish(#message{topic = Topic, from = Me, payload = Predicate}),
  ok.

  