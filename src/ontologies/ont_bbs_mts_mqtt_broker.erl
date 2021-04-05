%%%-------------------------------------------------------------------
%%% @author netboz
%%% @copyright (C) 2015, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 15. juil. 2015 23:23
%%%-------------------------------------------------------------------
-module(ont_bbs_mts_mqtt_broker).

-author("netboz").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API

-define(ERLANG_PREDS,
  [{{start_mqtt_internal, 1}, ?MODULE, start_mqtt_internal_predicate}]).

%% API
-export([external_predicates/0]).
-export([start_mqtt_internal_predicate/3]).

external_predicates() ->
  ?ERLANG_PREDS.

start_mqtt_internal_predicate({_Atom, StartResultIn}, Next0, #est{bs=Bindings, vn=Vn, cps = Cps} = St) ->
  StartResult = application:ensure_all_started(emqx),
  FailFun = fun (Lst) ->
    fail_start_mqtt_internal_predicate(Lst)
            end,
  Cp = #cp{type=compiled, data=FailFun, next=Next0, bs=Bindings, vn=Vn},

  erlog_int:unify_prove_body(StartResultIn, StartResult, Next0, St#est{cps = [Cp|Cps]}).


fail_start_mqtt_internal_predicate(St) ->
  application:stop(emqx),
  lager:info("Stopping broker",[]),
  erlog_int:fail(St).
