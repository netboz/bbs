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

%% Prolog API

-define(ERLANG_PREDS, [
  {{subscribe, 1}, ?MODULE, subscribe_predicate}
]).

%% API
-export([external_predicates/0]).
-export([subscribe_predicate/3]).

external_predicates() ->
  ?ERLANG_PREDS.


subscribe_predicate({_, Topic}, Next0, #est{bs=Bs} = St) ->
  case erlog_int:dderef(Topic, Bs) of
    {_} ->
      %% TODO manage unbinded Topic
      erlog_int:fail(st);
    Topic when is_binary(Topic) ->
      emqx:subscribe(Topic),
      erlog_int:prove_body(Next0,St)
  end.

%send_message({_, Destination, Acc, Ontology, Predicate})
%  Topic =