%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2020 23:24
%%%-------------------------------------------------------------------
-module(bbs_db_ets).

-author("Many").

-export([new/1]).
-export([add_built_in/2, add_compiled_proc/4, asserta_clause/4, assertz_clause/4]).
-export([retract_clause/3, abolish_clauses/2]).
-export([get_procedure/2, get_procedure_type/2]).
-export([get_interpreted_functors/1]).

%% new(InitArgs) -> Db.

new([NameSpace, AgentId]) ->
    Db = ets:new(
        binary_to_atom(iolist_to_binary([AgentId, "_", NameSpace]), utf8),
        [set, protected, {keypos, 1}]
    ),
    {NameSpace, Db}.

%% add_built_in(Functor, Database) -> NewDatabase.
%%  Add Functor as a built-in in the database.

add_built_in({NameSpace, Db}, Functor) ->
    ets:insert(Db, {Functor, built_in}),
    {NameSpace, Db}.

%% add_compiled_proc(Db, Functor, Module, Function) -> {ok,NewDb} | error.
%%  Add functor as a compiled procedure with code in M:F in the
%%  database. Check that it is not a built-in, if so return error.

add_compiled_proc({NameSpace, Db}, Functor, M, F) ->
    case ets:lookup(Db, Functor) of
        [{_, built_in}] ->
            error;
        _ ->
            ets:insert(Db, {Functor, code, {M, F}}),
            {ok, {NameSpace, Db}}
    end.

%% asserta_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%% assertz_clause(Db, Functor, Head, Body) -> {ok,NewDb} | error.
%%  We DON'T check format and just put it straight into the database.

asserta_clause({NameSpace, Db}, Functor, Head, Body) ->
    assert_clause_generic({NameSpace, Db}, Functor, Head, Body, a).

assertz_clause({NameSpace, Db}, Functor, Head, Body) ->
    assert_clause_generic({NameSpace, Db}, Functor, Head, Body, z).

assert_clause_generic({NameSpace, Db}, Functor, Head, Body, Pos) ->
    case {ets:lookup(Db, Functor), Pos} of
        {[{_, built_in}], _} ->
            error;
        {[{_, code, _}], _} ->
            error;
        {[{_, clauses, Tag, Cs}], z} ->
            ets:insert(Db, {Functor, clauses, Tag + 1, Cs ++ [{Tag, Head, Body}]}),
            {ok, {NameSpace, Db}};
        {[{_, clauses, Tag, Cs}], a} ->
            ets:insert(Db, {Functor, clauses, Tag + 1, [{Tag, Head, Body} | Cs]}),
            {ok, {NameSpace, Db}};
        {[], _} ->
            ets:insert(Db, {Functor, clauses, 1, [{0, Head, Body}]}),
            {ok, {NameSpace, Db}}
    end.

%% retract_clause(Db, Functor, ClauseTag) -> {ok,NewDb} | error.
%%  Retract (remove) the clause with tag ClauseTag from the list of
%%  clauses of Functor.

retract_clause({NameSpace, Db}, F, Ct) ->
    case ets:lookup(Db, F) of
        [{_, built_in}] ->
            error;
        [{_, code, _}] ->
            error;
        [{_, clauses, Nt, Cs}] ->
            ets:insert(Db, {F, clauses, Nt, lists:keydelete(Ct, 1, Cs)}),
            {ok, {NameSpace, Db}};
        [] ->
            %Do nothing
            {ok, {NameSpace, Db}}
    end.

%% abolish_clauses(Database, Functor) -> NewDatabase.

abolish_clauses({NameSpace, Db}, Func) ->
    case ets:lookup(Db, Func) of
        [{_, built_in}] ->
            error;
        [{_, code, _}] ->
            ets:delete(Db, Func),
            {ok, {NameSpace, Db}};
        [{_, clauses, _, _}] ->
            ets:delete(Db, Func),
            {ok, {NameSpace, Db}};
        [] ->
            %Do nothing
            {ok, {NameSpace, Db}}
    end.

%% get_procedure(Db, Functor) ->
%%        built_in | {code,{Mod,Func}} | {clauses,[Clause]} | undefined.
%% Return the procedure type and data for a functor.

get_procedure({_NameSpace, Db}, Functor) ->
    case ets:lookup(Db, Functor) of
        [{_, built_in}] ->
            built_in;
        [{_, code, C}] ->
            {code, C};
        [{_, clauses, _, Cs}] ->
            {clauses, Cs};
        [] ->
            undefined
    end.

%% get_procedure_type(Db, Functor) ->
%%        built_in | compiled | interpreted | undefined.
%%  Return the procedure type for a functor.

get_procedure_type({_NameSpace, Db}, Functor) ->
    case ets:lookup(Db, Functor) of
        [{_, built_in}] ->
            %A built-in
            built_in;
        [{_, code, _}] ->
            %Compiled (perhaps someday)
            compiled;
        [{_, clauses, _, _}] ->
            %Interpreted clauses
            interpreted;
        [] ->
            %Undefined
            undefined
    end.

%% get_interp_functors(Database) -> [Functor].

get_interpreted_functors({_NameSpace, Db}) ->
    NewDb =
        ets:foldl(
            fun
                ({Func, clauses, _, _}, Fs) ->
                    [Func | Fs];
                (_, Fs) ->
                    Fs
            end,
            [],
            Db
        ),
    NewDb.
