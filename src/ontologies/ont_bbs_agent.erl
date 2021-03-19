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
    {{new_knowledge_base, 1}, ?MODULE, new_knowledge_base_predicate},

    {{wait_for, 2}, ?MODULE, wait_for_predicate}
  ]).

%==============================================================================
% Exports
%==============================================================================

%% Built in predicate behaviour
-export([external_predicates/0]).

%% Built in predicates
-export([new_knowledge_base_predicate/3, wait_for_predicate/3]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Return the list of built in predicates contained in this module
%%
%% @end
%%------------------------------------------------------------------------------

external_predicates() ->
  ?ERLANG_PREDS.


new_knowledge_base_predicate({_Atom, NameSpace}, Next0, #est{} = St) ->
  AgentName = get(agent_name),
  case bbs_ontology:create_kb_store(NameSpace, AgentName, bbs_db_ets) of
    {ok, Est} ->
      case bbs_agent:store_ontology_state_on_namespace(NameSpace, Est) of
        undefined ->
          ?INFO_MSG("Crreated kb :~p",[NameSpace]),
          erlog_int:prove_body(Next0, St);
        _ ->
          %% an kb with same namespace already exists
          erlog_int:fail(St)
      end;
    {error, _Reason} ->
      erlog_int:fail(St)
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Stop current proof execution.
%% @end
%%------------------------------------------------------------------------------

wait_for_predicate({_Atom, NameSpace, PredicatePattern}, Next0, #est{bs = Bs} = St) ->
  %TODO: Manage backtracking
  DDPredPat = erlog_int:dderef(PredicatePattern, Bs),
  ?INFO_MSG("waiting  for ~p:~p", [NameSpace, DDPredPat]),
  case bbs_agent:get_ontology_state_from_namespace(<<"bbs:agent:stims">>) of
    #est{} = StimsKbState->
      case erlog_int:assertz_clause({':-', {stim, NameSpace, DDPredPat}, {restore_state, term_to_binary({Next0, St})}}, StimsKbState#est.db) of
        #db{} ->
          {{paused, NameSpace, DDPredPat}, Next0, St};
        _ ->
          erlog_int:fail(St)
      end;
    _ ->
      erlog_int:fail(St)
  end.


%-------------------------------------------------------------------------------
%  Utilities
%-------------------------------------------------------------------------------

