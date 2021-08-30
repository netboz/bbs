%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2020 23:23
%%%-------------------------------------------------------------------
-module(bbs_ontology).

-author("Yan").

-include_lib("erlog/include/erlog_int.hrl").

-include("bbs.hrl").
-include("utils.hrl").

%==============================================================================
% Exports
%==============================================================================

%% Ontologies registration/instanciation
-export([get_registered_ont_desc/1, register_ontologies/1, create_kb_store/3,
         build_ontology/3]).
% Built in predicates
-export([lager_predicate/3, prove_external_ontology_predicate/3, type_of_predicate/3]).
% Utilities
-export([prove/2, string_to_eterm/1, put_onto_hooks/3]).

%% Built-in (erlang) Predicates included into all ontologies that will be registered
-define(COMMON_BUILD_IN_PREDS,
        [{{log, 3}, ?MODULE, lager_predicate},
         {{'::', 2}, ?MODULE, prove_external_ontology_predicate},
         {{type_of, 2}, ?MODULE, type_of_predicate}]).
%% Predicates included into all ontologies that will be registered
-define(COMMON_PROLOG_PREDS, [{file, "common_predicates.pl"}]).

%%------------------------------------------------------------------------------
%% @doc
%% Register a list of ontologies availables to all bubbles on this node
%% @end
%%------------------------------------------------------------------------------

register_ontologies([]) ->
    ?INFO_MSG("Finished Ontologies registration.", []);
register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds} | Others])
    when is_list(OntoNameSpace) ->
    register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds} | Others]);
register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds} | Others]) ->
    ?INFO_MSG("Registering ontology :~p", [{OntoNameSpace, TextPreds, BuiltInPreds}]),
    case validate_preds(TextPreds, [], []) of
        {[], _Validated} ->
            ?INFO_MSG("~p :Validated all predicates.", [OntoNameSpace]);
        {Unvalidated, _} ->
            lists:foreach(fun({Error, El}) -> ?ERROR_MSG("~p : ~p", [El, Error]) end, Unvalidated)
    end,

    case validate_load_built_in_preds(BuiltInPreds, [], []) of
        {[], _ValidatedBIPs} ->
            ?INFO_MSG("~p :Validated all built in predicates.", [OntoNameSpace]);
        {UnvalidatedBIPS, _} ->
            lists:foreach(fun({Error, El}) -> ?ERROR_MSG("~p : ~p", [El, Error]) end,
                          UnvalidatedBIPS)
    end,
    ets:insert(?ONTO_STORE, {OntoNameSpace, TextPreds, BuiltInPreds}),
    register_ontologies(Others).

%%------------------------------------------------------------------------------
%% @doc
%% Assert the erlang modules that may be linked to an ontology were loaded
%% @end
%%------------------------------------------------------------------------------

validate_load_built_in_preds([], Failled, Succeeded) ->
    {Failled, Succeeded};
validate_load_built_in_preds([Mod | OtherMods], Failled, Succeeded) ->
    case validate_load_built_in_pred(Mod) of
        ok ->
            validate_load_built_in_preds(OtherMods, Failled, Succeeded ++ [Mod]);
        Error ->
            validate_load_built_in_preds(OtherMods, Failled ++ [{Error, Mod}], Succeeded)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Verfify the erlang module linked to the ontology is loaded.
%% @end
%%------------------------------------------------------------------------------

validate_load_built_in_pred(BuiltInPredMod) ->
    case code:ensure_loaded(BuiltInPredMod) of
        {module, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Assert the prolog predicates given as a string, or a filepath are
%% syntaxically corrects.
%% @end
%%------------------------------------------------------------------------------

validate_preds([], Failled, Succeeded) ->
    {Failled, Succeeded};
validate_preds([Pred | OtherPreds], Failled, Succeeded) ->
    case validate_text_pred(Pred) of
        ok ->
            validate_preds(OtherPreds, Failled, Succeeded ++ [Pred]);
        Error ->
            validate_preds(OtherPreds, Failled ++ [{Error, Pred}], Succeeded)
    end.

validate_text_pred({string, StrPred}) ->
    case string_to_eterm(StrPred) of
        {ok, _Terms} ->
            ok;
        Error ->
            Error
    end;
validate_text_pred({file, Filename}) ->
    PrivDir =
        case code:priv_dir(bbs) of
            {error, bad_name} ->
                % This occurs when not running as a release; e.g., erl -pa ebin
                % Of course, this will not work for all cases, but should account
                % for most
                "priv";
            SomeDir ->
                % In this case, we are running in a release and the VM knows
                % where the application (and thus the priv directory) resides
                % on the file system
                SomeDir
        end,
    AbsPath = filename:join([PrivDir, "ontologies", Filename]),
    case erlog_io:scan_file(AbsPath) of
        {ok, _Terms} ->
            ok;
        Error ->
            lager:info("Parse Error :~p", [Error]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return the description of an ontology upon the given namespace
%% syntaxically corrects.
%% @end
%%------------------------------------------------------------------------------

get_registered_ont_desc(NameSpace) ->
    ets:lookup(?ONTO_STORE, NameSpace).

%%------------------------------------------------------------------------------
%% @doc
%% Given a namespace and kb store module, fully build the initial ontology state
%% @end
%%------------------------------------------------------------------------------

-spec(build_ontology(AgentId :: binary(), NameSpace ::binary() | list(), DbMod :: atom()) -> tuple()).
build_ontology(AgentId, NameSpace, DbMod) when is_list(NameSpace) ->
    build_ontology(AgentId, iolist_to_binary(NameSpace), DbMod);
build_ontology(AgentId, NameSpace, DbMod) ->
    %% Get ontology detail from ontology repos
    case get_registered_ont_desc(NameSpace) of
        [] ->
            ?ERROR_MSG("Ontology not found :~p", [NameSpace]),
            {error, ontology_unavailable};
        [{_, _TextPredList, _BuiltInPredsList} = OntologyDescription] ->
            %% Create ontology Kb
            {ok, #est{} = InitialOntoState} = create_kb_store(NameSpace, AgentId, DbMod),
            %% TODO: Right now we return database table Id as ontology state, this needs to be changed
            Tid = InitialOntoState#est.db#db.ref,
            DbWithAPIs =
                lists:foldl(fun(M, Db) -> M:load(Db) end,
                            InitialOntoState#est.db,
                            [erlog_bips, erlog_lib_dcg, erlog_lib_lists]),
            %% Load Ontology predicates
            {ok, ReadyState} =
                load_ontology_predicates(OntologyDescription,
                                         InitialOntoState#est{db = DbWithAPIs}),
            ?INFO_MSG("Loaded all predicates for :~p", [NameSpace]),
            %% Load the predicates writen in erlang if needed, add them to Kb
            %%{ok, StateWithBuiltInPredicates} = load_built_in_predicates(OntologyDescription, StateWithBasePredicates),
            %% Load the predicates writen in Prolog, add them to Kb
            %%{ok, StateWithPrologPreds} = load_prolog_predicates(TextPredList, StateWithBuiltInPredicates),
            {ok, {Tid, ReadyState}}
    end.

create_kb_store(NameSpace, AgentId, DbMod) ->
    case erlog_int:new(DbMod, [NameSpace, AgentId]) of
        {ok, #est{} = Est} ->
            %% Load predicates common to all ontologies
            load_common_predicates(Est);
        {error, Reason} ->
            ?ERROR_MSG("Couldn't create ontology knowledge store:~p", [Reason]),
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Load a list of built-in predicates into ontology Kb
%%
%% @end
%%------------------------------------------------------------------------------

load_built_in_predicates(Built_in_predicates, #est{} = Est) ->
    NewDb =
        lists:foldl(fun({H, M, F}, Db) -> erlog_int:add_compiled_proc(H, M, F, Db) end,
                    Est#est.db,
                    Built_in_predicates),
    ?INFO_MSG("Loaded built-in predicates : ~p", [Built_in_predicates]),
    {ok, Est#est{db = NewDb}}.

%%------------------------------------------------------------------------------
%% @doc
%% Load predicates into ontology Kb.
%% Pls note that erlang built in predicates are not loaded by this function, use
%% load_built_in_predicates/2 instead.
%% @end
%%------------------------------------------------------------------------------

load_prolog_predicates([], Est) ->
    {ok, Est};
load_prolog_predicates([{file, File} | OtherPredicates], #est{} = Est) ->
    PrivDir =
        case code:priv_dir(bbs) of
            {error, bad_name} ->
                % This occurs when not running as a release; e.g., erl -pa ebin
                % Of course, this will not work for all cases, but should account
                % for most
                "priv";
            SomeDir ->
                % In this case, we are running in a release and the VM knows
                % where the application (and thus the priv directory) resides
                % on the file system
                SomeDir
        end,
    try erlog_file:consult(
            filename:nativename(
                filename:join([PrivDir, "ontologies", File])),
            Est)
    of
        {ok, #est{} = NewEst} ->
            ?INFO_MSG("Consulted : ~p",
                      [filename:nativename(
                           filename:join([PrivDir, "ontologies", File]))]),
            load_prolog_predicates(OtherPredicates, NewEst);
        Other ->
            ?WARNING_MSG("Unexpected result while loading predicates from file :~p",
                         [{File, Other}]),
            {error, failed_consulting_predicates_from_file}
    catch
        M:E ->
            ?ERROR_MSG("ERROR loading predicates from file :~p ~p", [M, E]),
            {error, failed_consulting_predicates_from_file}
    end;
load_prolog_predicates([{text, TextualPrologPredicate} | OtherPredicates],
                       #est{} = Est) ->
    case string_to_eterm(TextualPrologPredicate) of
        {error, Error} ->
            ?ERROR_MSG("Error ~p while parsing : ~p ", [Error, TextualPrologPredicate]),
            {error, Error};
        TermPredicate when is_tuple(TermPredicate) ->
            NewDb = load_term_predicate(TermPredicate, Est),
            load_prolog_predicates(OtherPredicates, Est#est{db = NewDb})
    end;
load_prolog_predicates([Unk | OtherPredicates], Db) ->
    ?WARNING_MSG("Unrecognized intialisation predicate :~p", [Unk]),
    load_prolog_predicates(OtherPredicates, Db).

%%------------------------------------------------------------------------------
%% @doc
%% Insert into ontology a new predicate in internal prolog form
%% @end
%%------------------------------------------------------------------------------

load_term_predicate(TermPredicate, #est{} = State) ->
    erlog_int:asserta_clause(TermPredicate, State#est.db).

%%------------------------------------------------------------------------------
%% @doc
%% Load built-in and prolog predicates common to all ontologies
%% @end
%%------------------------------------------------------------------------------

load_common_predicates(InitialOntoState) ->
    {ok, StateWithCommonBuiltIn} =
        load_built_in_predicates(?COMMON_BUILD_IN_PREDS, InitialOntoState),
    {ok, _StateWithCommonPredicates} =
        load_prolog_predicates(?COMMON_PROLOG_PREDS, StateWithCommonBuiltIn).

%%------------------------------------------------------------------------------
%% @doc
%% Load built-in and prolog predicates common to all ontologies
%% @end
%%------------------------------------------------------------------------------

load_ontology_predicates({_, TextPredList, BuiltInModulesPredsList},
                         StateWithCommonPredicates) ->
    {ok, StateWithOntologyBuiltIn} =
        load_ontology_built_in_predicates(BuiltInModulesPredsList, StateWithCommonPredicates),
    {ok, _StateWithOntologPredicates} =
        load_prolog_predicates(TextPredList, StateWithOntologyBuiltIn).

%%------------------------------------------------------------------------------
%% @doc
%% Load built-in predicates for an ontology
%% @end
%%------------------------------------------------------------------------------

load_ontology_built_in_predicates([], #est{} = Kb) ->
    {ok, Kb};
load_ontology_built_in_predicates([PredicatesModule | OtherModules], Kb)
    when is_atom(PredicatesModule) ->
    case load_ontology_built_in_predicate(PredicatesModule, Kb) of
        {ok, #est{} = NewKb} ->
            load_ontology_built_in_predicates(OtherModules, NewKb);
        {error, Reason} ->
            ?ERROR_MSG("Built in predicates module :~p could not be loaded, reason:~p",
                       [PredicatesModule, Reason]),
            {error, Reason}
    end.

load_ontology_built_in_predicate(OntMod, Kb) when is_atom(OntMod) ->
    try OntMod:external_predicates() of
        BuiltInPredicate when is_list(BuiltInPredicate) ->
            load_built_in_predicates(BuiltInPredicate, Kb);
        Else ->
            Else
    catch
        Else ->
            ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [Else]),
            {error, Else};
        M:E ->
            ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [{M, E}]),
            {error, {M, E}}
    end.

%==============================================================================
% Built in predicates common to all ontologies
%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Call to Lager logging api from Prolog
%% @end
%%------------------------------------------------------------------------------

lager_predicate({_Atom, Type, Label, Arguments}, Next0, #est{bs = Bs} = St) ->
    case Type of
        info ->
            ?INFO_MSG(Label, erlog_int:dderef(Arguments, Bs));
        debug ->
            ?DEBUG(Label, erlog_int:dderef(Arguments, Bs));
        error ->
            ?ERROR_MSG(Label, erlog_int:dderef(Arguments, Bs));
        warning ->
            ?WARNING_MSG(Label, erlog_int:dderef(Arguments, Bs));
        _ ->
            ?ERROR_MSG("Unrecognized log level.", [])
    end,
    erlog_int:prove_body(Next0, St).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

prove_external_ontology_predicate({_Atom,
                                   ExternalOntologyNameSpace,
                                   ExternalOntologyPredicate},
                                  Next0,
                                  #est{bs = ParentBindings,
                                       cps = ParentCps,
                                       vn = ParentVn} =
                                      ParentOntologyState) ->
    DDExternalOntNs = erlog_int:dderef(ExternalOntologyNameSpace, ParentBindings),

    case bbs_agent:get_ontology_state_from_namespace(DDExternalOntNs) of
        #est{} = ExternalOntologyState ->

            DDExternalPredicate = erlog_int:dderef(ExternalOntologyPredicate, ParentBindings),
            case erlog_int:prove_goal(DDExternalPredicate,
                                      [],
                                      ExternalOntologyState#est{bs = erlog_int:new_bindings(),
                                                                vn = ParentVn})
            of
                {succeed, #est{vn = NewVn} = NewExternalState} ->
                    DDResultPredicate =
                        erlog_int:dderef(DDExternalPredicate, NewExternalState#est.bs),
                    {succeed, NewBindings} =
                        erlog_int:unify(DDResultPredicate,
                                        ExternalOntologyPredicate,
                                        ParentBindings),
                    FailFun =
                        fun(LCp, LCps, Lst) ->
                           fail_external_predicate({LCp, LCps, Lst},
                                                   Next0,
                                                   ParentOntologyState,
                                                   DDExternalPredicate,
                                                   NewExternalState)
                        end,
                    Cp = #cp{type = compiled,
                             data = FailFun,
                             next = Next0,
                             bs = ParentBindings,
                             vn = ParentVn},

                    erlog_int:prove_body(Next0,
                                         ParentOntologyState#est{cps = [Cp | ParentCps],
                                                                 bs = NewBindings,
                                                                 vn = NewVn});
                {fail, #est{} = _NewExternalState} ->
                    erlog_int:fail(ParentOntologyState);
                OtherResult ->
                    ?INFO_MSG("Unexpected result : ~p", [OtherResult]),
                    %% Any other result stops the current proof. This permits 'paused' predicate to stop the execution.
                    OtherResult
            end;
        undefined ->
            ?ERROR_MSG("External ontology not found : ~p", [DDExternalOntNs]),
            erlog_int:fail(ParentOntologyState)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% fail function to backtrack '::'
%%
%% @end
%%------------------------------------------------------------------------------

fail_external_predicate({_LCp, _LCps, _Lst},
                        Next0,
                        ParentOntologyState,
                        ExternalPredicate,
                        ExternalState) ->
    %?INFO_MSG("Failling :~p~n   ~p~n    ~p  ~p   ~p",[_LCp, _LCps, _Lst, Next0, ParentOntologyState]),
    case erlog_int:fail(ExternalState) of
        {succeed, #est{vn = NewVn} = NewExternalState} ->
            DDResultPredicate = erlog_int:dderef(ExternalPredicate, NewExternalState#est.bs),
            {succeed, NewBindings} =
                erlog_int:unify(DDResultPredicate, ExternalPredicate, ParentOntologyState#est.bs),
            erlog_int:prove_body(Next0, ParentOntologyState#est{bs = NewBindings, vn = NewVn});
        {fail, #est{}} ->
            erlog_int:fail(ParentOntologyState)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Predicate used to get type of a term.
%%
%% @end
%%------------------------------------------------------------------------------

type_of_predicate({_Atom, Term, Type}, Next0, #est{bs = Bs} = St) ->
    type_of_predicate_deref(erlog_int:dderef(Term, Bs), Type, Next0, St).

type_of_predicate_deref({'_'}, Type, Next0, #est{} = St) ->
    erlog_int:unify_prove_body(unbinded, Type, Next0, St);
type_of_predicate_deref(Term, Type, Next0, #est{} = St) when is_binary(Term) ->
    erlog_int:unify_prove_body(string, Type, Next0, St);
type_of_predicate_deref(Term, Type, Next0, #est{} = St) when is_tuple(Term) ->
    erlog_int:unify_prove_body(predicate, Type, Next0, St);
type_of_predicate_deref(Term, Type, Next0, #est{} = St) when is_number(Term) ->
    erlog_int:unify_prove_body(number, Type, Next0, St);
type_of_predicate_deref(Term, Type, Next0, #est{} = St) when is_list(Term) ->
    erlog_int:unify_prove_body(list, Type, Next0, St);
type_of_predicate_deref(_, Type, Next0, #est{} = St) ->
    erlog_int:unify_prove_body(unknown_type, Type, Next0, St).

%-------------------------------------------------------------------------------
%  Utilities
%-------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Add a hook ( namespace:predicate(parameters) ) to react to.
%% @end
%%------------------------------------------------------------------------------

%% todo: review.
put_onto_hooks({Ns, Functor}, {ArgPat, Next0, State}, Options)
    when is_record(State, est) ->
    ?INFO_MSG("setting hook on :~p", [{Ns, Functor}]),
    case get({reaction, Ns, Functor}) of
        undefined ->
            put({reaction, Ns, Functor}, [{ArgPat, Next0, State, Options}]);
        [] ->
            put({reaction, Ns, Functor}, [{ArgPat, Next0, State, Options}]);
        List ->
            put({reaction, Ns, Functor}, List ++ [{ArgPat, Next0, State, Options}])
    end;
put_onto_hooks({Ns, Functor}, {ArgPat, NsReact, ReactPred}, Options) when is_binary(Ns) ->
    case get({reaction, Ns, Functor}) of
        undefined ->
            put({reaction, Ns, Functor}, [{ArgPat, NsReact, ReactPred, Options}]);
        [] ->
            put({reaction, Ns, Functor}, [{ArgPat, NsReact, ReactPred, Options}]);
        List ->
            put({reaction, Ns, Functor}, List ++ [{ArgPat, NsReact, ReactPred, Options}])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Convert a predicate string to its eterm representation
%% @end
%%------------------------------------------------------------------------------

-spec string_to_eterm(list()) -> tuple.
string_to_eterm(String) ->
    case erlog_scan:tokens([], String, 1) of
        {done, {ok, Tokk, _}, _} ->
            case erlog_parse:term(Tokk) of
                {ok, Eterms} ->
                    Eterms;
                Other1 ->
                    {error, Other1}
            end;
        Other ->
            {error, Other}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Prove Namespace::Predicate in ontology stored into Kb
%% Not sure if this function will be kept
%% @end
%%------------------------------------------------------------------------------

prove(NameSpace, Predicate) when is_binary(NameSpace) ->
    ?INFO_MSG("Proving :~p", [{NameSpace, Predicate}]),
    case bbs_agent:get_ontology_state_from_namespace(NameSpace) of
        #est{} = Kb ->
            prove(Kb, Predicate);
        undefined ->
            ?WARNING_MSG("No kb :~p ", [{NameSpace, Predicate, erlang:process_info(self(), dictionary)}]),
            fail
    end;
prove(Kb, Predicate) ->
    erlog_int:prove_goal(Predicate, Kb).
