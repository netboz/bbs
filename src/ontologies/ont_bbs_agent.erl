%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2020, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2020 5:54 PM
%%%-------------------------------------------------------------------
-module(ont_bbs_agent).
-author("yan").

-include("bbs.hrl").
-include("utils.hrl").
-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API

-define(ERLANG_PREDS,
  [
    {{react_on, 3}, ?MODULE, react_on_predicate},
    {{wait_for, 2}, ?MODULE, wait_for_predicate}
  ]).

%==============================================================================
% Exports
%==============================================================================

%% Built in predicate behaviour
-export([external_predicates/0]).

%% Built in predicates
-export([react_on_predicate/3, wait_for_predicate/3]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Return the list of built in predicates contained in this module
%%
%% @end
%%------------------------------------------------------------------------------

external_predicates() ->
  ?ERLANG_PREDS.

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Stop current proof execution.
%% @end
%%------------------------------------------------------------------------------

wait_for_predicate({_Atom, NameSpace, PredicatePattern}, Next0, #est{bs = Bs} = St) ->
  %TODO: Manage backtracking
  DDPredPat = erlog_int:dderef(PredicatePattern, Bs),
  ?INFO_MSG("waiting  for ~p:~p", [NameSpace, DDPredPat]),
  PredPatList = tuple_to_list(DDPredPat),
  [PredPatFunc|PredPatParams] = PredPatList,
  {{paused, NameSpace, PredPatFunc, PredPatParams}, Next0, St}.

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Register a predicate to react on into agent state.
%% @end
%%------------------------------------------------------------------------------
react_on_predicate({Atom, NameSpace, PredPat, PredReact}, Next0, #est{} = St) ->
  {NsReact, _Db} = St#est.db#db.ref,
  react_on_predicate({Atom, NameSpace, PredPat, NsReact, PredReact}, Next0, St);

react_on_predicate({Atom, NameSpace, PredPat, NsReact, PredReact}, Next0, #est{} = St) ->
  react_on_predicate({Atom, NameSpace, PredPat, NsReact, PredReact, []}, Next0, St);

react_on_predicate({_Atom, NameSpace, PredPat, NsReact, PredReact, Options}, Next0, #est{bs = Bs} = St) ->
  react_on(NameSpace, PredPat, NsReact, PredReact, Options, Bs),
  erlog_int:prove_body(Next0, St).

react_on(NameSpaceIn, PredPat, NameSpaceOut, PredReact, Options, Bindings) ->
  DDPredPat = erlog_int:dderef(PredPat, Bindings),
  DDPredReact = erlog_int:dderef(PredReact, Bindings),
  PredPatList = tuple_to_list(DDPredPat),
  [PredPatFunc|PredPatParams] = PredPatList,
  put_onto_hooks({NameSpaceIn, PredPatFunc}, {PredPatParams, NameSpaceOut, DDPredReact}, Options).


%-------------------------------------------------------------------------------
%  Utilities
%-------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Add a hook ( namespace:predicate(parameters) ) to react to.
%% @end
%%------------------------------------------------------------------------------

%% todo: review.
put_onto_hooks({Ns, Functor}, {ArgPat, Next0, State}, Options) when is_record(State, est) ->
  ?INFO_MSG("setting hook on :~p",[{Ns,Functor}]),
  case get({reaction, Ns, Functor}) of
    undefined ->
      put({reaction, Ns, Functor},  [{ArgPat, Next0, State, Options}]);
    [] ->
      put({reaction, Ns, Functor},  [{ArgPat, Next0, State, Options}]);
    List ->
      put({reaction, Ns, Functor},  List ++ [{ArgPat, Next0, State, Options}])
  end;

put_onto_hooks({Ns, Functor}, {ArgPat, NsReact, ReactPred}, Options) when is_binary(Ns) ->
  case get({reaction, Ns, Functor}) of
    undefined ->
      put({reaction, Ns, Functor},  [{ArgPat, NsReact, ReactPred, Options}]);
    [] ->
      put({reaction, Ns, Functor},  [{ArgPat, NsReact, ReactPred, Options}]);
    List ->
      put({reaction, Ns, Functor},  List ++ [{ArgPat, NsReact, ReactPred, Options}])
  end.