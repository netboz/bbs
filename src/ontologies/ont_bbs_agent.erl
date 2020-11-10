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
-export([react_on_predicate/3, wait_for_predicate/3, prove_external_ontology_predicate/3]).

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

wait_for_predicate({_Atom, NameSpace, PredPat},Next0,#est{} = St) ->
  %TODO: Manage backtracking
  ?INFO_MSG("waiting  for ~p:~p", [NameSpace, PredPat]),
  PredPatList = tuple_to_list(PredPat),
  [PredPatFunc|PredPatParams] = PredPatList,
  {{paused, NameSpace, PredPatFunc, PredPatParams}, Next0, St}.

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Register a predicate to react on into agent state.
%% @end
%%------------------------------------------------------------------------------

react_on_predicate({_Atom, NameSpace, PredPat, NsReact, PredReact, Options}, Next0, #est{} = St) ->
  react_on(NameSpace, PredPat, NsReact, PredReact, Options),
  erlog_int:prove_body(Next0, St);

react_on_predicate({_Atom, NameSpace, PredPat, PredReact}, Next0, #est{} = St) ->
  {NsReact, _Db} = St#est.db#db.ref,
  react_on(NameSpace, PredPat, NsReact, PredReact),
  erlog_int:prove_body(Next0, St);

react_on_predicate({_Atom, NameSpace, PredPat, PredReact, Options}, Next0, #est{} = St) ->
  {_AgName, NsReact} = St#est.db#db.ref,
  react_on(NameSpace, PredPat, NsReact, PredReact, Options),
  erlog_int:prove_body(Next0, St).

react_on(NameSpaceIn, PredPat, NameSpaceOut, PredReact) ->
  react_on(NameSpaceIn, PredPat, NameSpaceOut, PredReact,[]).

react_on(NameSpaceIn, PredPat, NameSpaceOut, PredReact, Options) ->
  PredPatList = tuple_to_list(PredPat),
  [PredPatFunc|PredPatParams] = PredPatList,
  put_onto_hooks({NameSpaceIn, PredPatFunc}, {PredPatParams, NameSpaceOut, PredReact}, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

prove_external_ontology_predicate({_Atom, ExternalOntologyNameSpace, ExternalOntologyPredicate},
    Next0, #est{} = ParentOntologyState) ->
  case bbs_agent:get_onto_ns(ExternalOntologyNameSpace) of
    #est{} = ExternalOntologyState ->
      case erlog_int:prove_goal(ExternalOntologyPredicate, ExternalOntologyState) of
        %{{succeed, [{'Z', Value}]}, NewState}
        {{succeed, Bindings}, _NewExternalState} ->
          ?INFO_MSG("Succeeded external predicate : ~p",[{ExternalOntologyPredicate, Bindings, Next0}]),
          _ExternalCp = #cp{};
        fail ->
          erlog_int:fail(ParentOntologyState)
      end;
    undefined ->
      erlog_int:fail(ParentOntologyState)
  end.


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