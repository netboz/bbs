%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, qengho
%%% @doc FSM describing a bbs agent
%%%
%%% @end
%%% Created : 10. Jan 2020 23:22
%%%-------------------------------------------------------------------
-module(bbs_agent).

-author("Yan").

-include("bbs.hrl").
-include("utils.hrl").


-export([get_ontology_state_from_namespace/1, store_ontology_state_on_namespace/2, prove/2]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

prove(NameSpace, Predicate) ->
    bbs_ontology:prove(NameSpace, Predicate).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%%  Get an raw ontology state stored under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec get_ontology_state_from_namespace(Ns :: binary()) -> undefined | tuple().
get_ontology_state_from_namespace(Ns) ->
    get(Ns).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Store an ontology state under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec store_ontology_state_on_namespace(Ns :: binary(), Kb :: term()) ->
    undefined | term().
store_ontology_state_on_namespace(Ns, KbDb) ->
    put(Ns, KbDb).

