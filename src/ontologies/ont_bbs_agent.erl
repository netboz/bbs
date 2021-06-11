%%%-------------------------------------------------------------------
%%% @author Yan
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
        [%% Ontology related predicates
         {{new_knowledge_base, 1}, ?MODULE, new_knowledge_base_predicate},
         {{new_knowledge_base, 2}, ?MODULE, new_knowledge_base_predicate},
         {{prolog_interpretation, 2}, ?MODULE, prolog_interpretation_predicate}
        ]).

%==============================================================================
% Exports
%==============================================================================

%% Built in predicate behaviour
-export([external_predicates/0]).
%% Built in predicates
-export([new_knowledge_base_predicate/3, prolog_interpretation_predicate/3]).
-export([string_to_eterm/1]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Return the list of built in predicates contained in this module.
%% @end
%%------------------------------------------------------------------------------

external_predicates() ->
    ?ERLANG_PREDS.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Predicate creating a new 'empty' Kb ready for use.
%% It's not really empty as basic predicates like assert are included.
%% @end
%%------------------------------------------------------------------------------

new_knowledge_base_predicate({_Atom, NameSpace}, Next0, #est{} = St) ->
    AgentName = get(agent_name),
    case bbs_ontology:create_kb_store(NameSpace, AgentName, bbs_db_ets) of
        {ok, Est} ->
            case bbs_agent:store_ontology_state_on_namespace(NameSpace, Est) of
                undefined ->
                    ?INFO_MSG("Crreated kb :~p", [NameSpace]),
                    erlog_int:prove_body(Next0, St);
                _ ->
                    %% an kb with same namespace already exists
                    erlog_int:fail(St)
            end;
        {error, _Reason} ->
            erlog_int:fail(St)
    end;
new_knowledge_base_predicate({_Atom, NameSpace, Db_mod}, Next0, #est{} = St) ->
    AgentName = get(agent_name),
    case bbs_ontology:create_kb_store(NameSpace, AgentName, Db_mod) of
        {ok, Est} ->
            case bbs_agent:store_ontology_state_on_namespace(NameSpace, Est) of
                undefined ->
                    ?INFO_MSG("Crreated kb :~p", [NameSpace]),
                    erlog_int:prove_body(Next0, St);
                _ ->
                    %% an kb with same namespace already exists
                    erlog_int:fail(St)
            end;
        {error, _Reason} ->
            erlog_int:fail(St)
    end.

%-------------------------------------------------------------------------------
%  Utility predicates
%-------------------------------------------------------------------------------

prolog_interpretation_predicate({_Atom, PrologString, PrologTerms},
                                Next0,
                                #est{bs = Bs, vn = Vn} = St) ->
    DPrologString = erlog_int:dderef(PrologString, Bs),
    PrologTerms = erlog_int:dderef(PrologTerms, Bs),
    ?INFO_MSG("Starting convertion predicate", []),
    case DPrologString of
        String when is_binary(String) ->
            % PrologString is binded to a string, try to convert it
            case string_to_eterm(binary_to_list(String) ++ " ") of
                [error, Error] ->
                    ?WARNING_MSG("Couln't convert to eterm : ~p", [Error]),
                    erlog_int:fail(St);
                Terms ->
                    Vs = vars_in(Terms),
                    %% Goal may be a list of goals, ensure proper goal.
                    Goal0 = unlistify(Terms),
                    ?INFO_MSG("Goal converted : ~p", [{Goal0, Vs}]),

                    {Goal1, NewBs, NewVn} = erlog_int:initial_goal(Goal0, Bs, Vn),

                    %% Convertion went fine
                    erlog_int:unify_prove_body(PrologTerms,
                                               Goal1,
                                               Next0,
                                               St#est{bs = NewBs, vn = NewVn})
            end;
        _ ->
            %% Prolog String is either unbinded or something else than a string
            %% if not binded, convertion from pterms to prolog is a TODO.
            %% If it is of another type than a string, we fail also.
            erlog_int:fail(St)
    end.

string_to_eterm(String) ->
    %% We add a space at the end of the string to parse because of a probable error in prolog parser
    %% A list is returned in case of error to avoid eterm confusion if we return {error, Error}
    case erlog_scan:tokens([], String, 1) of
        {done, {ok, Tokk, _}, _} ->
            case erlog_parse:term(Tokk) of
                {ok, Eterms} ->
                    Eterms;
                Other1 ->
                    [error, Other1]
            end;
        Other ->
            [error, Other]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% vars_in(Term) -> [{Name,Var}].
%% Returns an ordered list of {VarName,Variable} pairs.

vars_in(Term) ->
    vars_in(Term, orddict:new()).

vars_in({'_'}, Vs) ->
    Vs;                       %Never in!
vars_in({Name} = Var, Vs) ->
    orddict:store(Name, Var, Vs);
vars_in(Struct, Vs) when is_tuple(Struct) ->
    vars_in_struct(Struct, 2, tuple_size(Struct), Vs);
vars_in([H | T], Vs) ->
    vars_in(T, vars_in(H, Vs));
vars_in(_, Vs) ->
    Vs.

vars_in_struct(_Str, I, S, Vs) when I > S ->
    Vs;
vars_in_struct(Str, I, S, Vs) ->
    vars_in_struct(Str, I + 1, S, vars_in(element(I, Str), Vs)).

unlistify([G]) ->
    G;
unlistify([G | Gs]) ->
    {',', G, unlistify(Gs)};
unlistify([]) ->
    true;
unlistify(G) ->
    G.                              %In case it wasn't a list.
