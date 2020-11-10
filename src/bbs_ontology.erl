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
-export([get_registered_ont_desc/1, register_ontologies/1, build_ontology/3]).

% Built in predicates
-export([lager_predicate/3, prove_external_ontology_predicate/3]).

% Utilities
-export([prove/3, string_to_eterm/1]).

%% Predicates included into all ontologies that will be registered
-define(COMMON_BUILD_IN_PREDS, [
  {{log, 3}, ?MODULE, lager_predicate},
  {{'::', 2}, ?MODULE, prove_external_ontology_predicate}
]).

%%------------------------------------------------------------------------------
%% @doc
%% Register a list of ontologies availables to all bubbles on this node
%% @end
%%------------------------------------------------------------------------------

register_ontologies([]) ->
  ?INFO_MSG("Finished Ontologies registration.",[]);

register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds}|Others]) when is_list(OntoNameSpace) ->
  register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds}|Others]);

register_ontologies([{OntoNameSpace, TextPreds, BuiltInPreds}|Others]) ->
  ?INFO_MSG("Registering ontology :~p",[{OntoNameSpace, TextPreds, BuiltInPreds}]),
  case validate_preds(TextPreds,[],[]) of
    {[],_Validated} ->
      ?INFO_MSG("~p :Validated all predicates.",[OntoNameSpace]);
    {Unvalidated, _} ->
      lists:foreach(fun({Error, El}) -> ?ERROR_MSG("~p : ~p",[El, Error ]) end, Unvalidated)
  end,

  case validate_load_built_in_preds(BuiltInPreds,[],[]) of
    {[],_ValidatedBIPs} ->
      ?INFO_MSG("~p :Validated all built in predicates.",[OntoNameSpace]);

    {UnvalidatedBIPS, _} ->
      lists:foreach(fun({Error, El}) -> ?ERROR_MSG("~p : ~p",[El, Error ]) end, UnvalidatedBIPS)
  end,
  ets:insert(?ONTO_STORE,{OntoNameSpace, TextPreds, BuiltInPreds}),
  register_ontologies(Others).

%%------------------------------------------------------------------------------
%% @doc
%% Assert the erlang modules that may be linked to an ontology were loaded
%% @end
%%------------------------------------------------------------------------------

validate_load_built_in_preds([],Failled, Succeeded) ->
  {Failled, Succeeded};
validate_load_built_in_preds([Mod|OtherMods],Failled, Succeeded) ->
  case validate_load_built_in_pred(Mod) of
    ok ->
      validate_load_built_in_preds(OtherMods, Failled,Succeeded ++ [Mod]);
    Error ->
      validate_load_built_in_preds(OtherMods, Failled ++ [{Error,Mod}],Succeeded)
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
      {error,Error}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Assert the prolog predicates given as a string, or a filepath are
%% syntaxically corrects.
%% @end
%%------------------------------------------------------------------------------

validate_preds([], Failled, Succeeded) ->
  {Failled, Succeeded};

validate_preds([Pred|OtherPreds], Failled, Succeeded) ->
  case validate_text_pred(Pred) of
    ok ->
      validate_preds(OtherPreds, Failled, Succeeded ++ [Pred]);
    Error ->
      validate_preds(OtherPreds, Failled ++ [{Error, Pred}], Succeeded)
  end.

validate_text_pred({string, StrPred}) ->
  case string_to_eterm(StrPred) of
    {ok,_Terms} ->
      ok;
    Error ->
      Error
  end ;

validate_text_pred({file, Filename}) ->
  PrivDir = case code:priv_dir(bbs) of
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
    {ok,_Terms} ->
      ok;
    Error ->
      lager:info("Parse Error :~p",[Error]),
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


%% Load predicates common to all ontologies

load_common_predicates(#est{} = Est) ->
  NewDb = lists:foldl(fun({Head, M, F}, Db) ->
    erlog_int:add_compiled_proc(Head, M, F, Db) end, Est#est.db,
    ?COMMON_BUILD_IN_PREDS),
  ?INFO_MSG("Loaded global predicates : ~p", [?COMMON_BUILD_IN_PREDS]),

  {ok, Est#est{db = NewDb}}.

%% TODO : Permit loading multiple prolog files

%% Load prolog files in text format

load_prolog_predicates([], Est) ->
  {ok, Est};

%%load_prolog_predicates([{string, Predicate}|Other_preds], #est{} = Est) ->
%%  %% Return our Kb with all clauses loaded
%%  try erlog_int:assertz_clause(string_to_eterm(Predicate), Est#est.db) of
%%
%%  end,
%%  load_prolog_predicates(Other_preds, Est#est{db = NewDb});

load_prolog_predicates([{file, File}|OtherPredicates], #est{} = Est) ->

  PrivDir = case code:priv_dir(bbs) of
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
  ?INFO_MSG("Consulting : ~p", [filename:nativename(filename:join([PrivDir, "ontologies", File]))]),

  try erlog_file:consult(filename:nativename(filename:join([PrivDir, "ontologies", File])), Est) of
    {ok, #est{} = NewEst} ->
      ?INFO_MSG("Consulted : ~p", [filename:nativename(filename:join([PrivDir, "ontologies", File]))]),
      load_prolog_predicates(OtherPredicates, NewEst);
    Other ->
      ?WARNING_MSG("Unexpected result while loading predicates from file :~p", [{File, Other}]),
      {error, failed_consulting_predicates_from_file}
  catch
    M:E ->
      ?INFO_MSG("ERROR loading predicates from file :~p ~p",[M,E]),
      {error, failed_consulting_predicates_from_file}
  end;

load_prolog_predicates([Unk|OtherPredicates], Db) ->
  ?WARNING_MSG("Unrecognized intialisation predicate :~p", [Unk]),
  load_prolog_predicates(OtherPredicates, Db).

%%------------------------------------------------------------------------------
%% @doc
%% Load built-in predicates from erlang module
%% @end
%%------------------------------------------------------------------------------

load_built_in_predicates([], Kb) ->
  {ok, Kb};

load_built_in_predicates([PredicatesModule|OtherModules], Kb) ->
  case load_built_in_predicate(PredicatesModule,Kb) of
    {ok, #est{} = NewKb} ->
      load_built_in_predicates(OtherModules, NewKb);
    {error, Reason} ->
      ?ERROR_MSG("Built in predicates module :~p could not be loaded, reason:~p",
        [PredicatesModule, Reason]),
      {error, Reason}
  end.

load_built_in_predicate(OntMod, Kb) when is_atom(OntMod) ->
  try OntMod:external_predicates() of
    Result when is_list(Result) ->
      lists:foldl(fun({Head, M, F}, Db) ->
        erlog_int:add_compiled_proc(Head, M, F, Db) end, Kb#est.db,
        Result),
      ?INFO_MSG("Added external predicates from : ~p, predicates : ~p", [OntMod, Result]),
      {ok,Kb};
    Else -> Else
  catch
    Else ->
      ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [Else]),
      {error, Else};
    M:E ->
      ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [{M, E}]),
      {error, {M, E}}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Given a namespace and kb store module, fully build the initial ontology state
%% @end
%%------------------------------------------------------------------------------

build_ontology(AgentId, NameSpace, DbMod) when is_list(NameSpace) ->
  build_ontology(AgentId,iolist_to_binary(NameSpace), DbMod);

build_ontology(AgentId, NameSpace, DbMod) ->
  %% Get ontology detail from ontology repos
  case get_registered_ont_desc(NameSpace) of
    [] ->
      ?ERROR_MSG("Ontology not found :~p",[NameSpace]),
      {error, ontology_unavailable};
    [{_, TextPredList, BuiltInPredsList}] ->
      %% Create ontology Kb
      {ok, #est{} = InitialOntoState} = create_ontology_store(NameSpace, AgentId, DbMod),
      %% TODO: Right now we return database table Id as ontology state, this needs to be changed
      Tid = InitialOntoState#est.db#db.ref,
      {ok, StateWithBasePredicates} = load_common_predicates(InitialOntoState),
      %% Load the predicates writen in erlang if needed, add them to Kb
      {ok, StateWithBuiltInPredicates} = load_built_in_predicates(BuiltInPredsList, StateWithBasePredicates),
      %% Load the predicates writen in Prolog, add them to Kb
      {ok, StateWithPrologPreds} = load_prolog_predicates(TextPredList, StateWithBuiltInPredicates),
      {ok, {Tid, StateWithPrologPreds}}
  end.

create_ontology_store(NameSpace, AgentId, DbMod) ->
  case erlog_int:new(DbMod, [NameSpace, AgentId])  of
    {ok, #est{} = Est} ->
      {ok, Est};
    {error, Reason} ->
      ?ERROR_MSG("Couldn't create ontology knowledge store:~p",[Reason])
  end.

%==============================================================================
% Built in predicates common to all ontologies
%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Built in predicate. Call to Lager logging api from Prolog
%% @end
%%------------------------------------------------------------------------------

lager_predicate({_Atom, Type, Label, Arguments}, Next0, #est{} = St) ->
  case Type of
    info ->
      ?INFO_MSG(Label, Arguments);
    debug ->
      ?DEBUG(Label, Arguments);
    error ->
      ?ERROR_MSG(Label, Arguments);
    warning ->
      ?WARNING_MSG(Label, Arguments);
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

prove_external_ontology_predicate({_Atom, ExternalOntologyNameSpace, ExternalOntologyPredicate},
    Next0, #est{bs=ParentBindings, cps=ParentCps, vn=ParentVn} = ParentOntologyState) ->
  %?INFO_MSG("Proving External : ~p   bindings :~p",[{ExternalOntologyNameSpace, ExternalOntologyPredicate}, dict:to_list(ParentBindings)]),
  %?INFO_MSG("deref predicate :~p",[erlog_int:deref(ExternalOntologyPredicate, ParentBindings)]),
  case bbs_agent:get_ontology_state_from_namespace(ExternalOntologyNameSpace) of
    #est{} = ExternalOntologyState ->
      DDExternalPredicate = erlog_int:dderef(ExternalOntologyPredicate, ParentBindings),
      case erlog_int:prove_body([DDExternalPredicate], ExternalOntologyState#est{bs = erlog_int:new_bindings()}) of
        {succeed, NewExternalState} ->
          %?INFO_MSG("Succeeded external predicate : ~p",[{ExternalOntologyPredicate, NewExternalState#est.bs, Next0}]),
          DDResultPredicate = erlog_int:dderef(DDExternalPredicate, NewExternalState#est.bs),
          %?INFO_MSG("DDref external :~p",[DDResultPredicate]),
          {succeed, NewBindings} = erlog_int:unify(DDResultPredicate, ExternalOntologyPredicate, ParentBindings),
          FailFun = fun (LCp, LCps, Lst) ->
            fail_external_predicate({LCp, LCps, Lst}, Next0, ParentOntologyState, DDExternalPredicate, NewExternalState)
                    end,
          Cp = #cp{type=compiled,data=FailFun, next=Next0, bs=NewBindings, vn=ParentVn},
          erlog_int:prove_body(Next0, ParentOntologyState#est{cps = [Cp|ParentCps], bs = NewBindings});
        fail ->
          ?INFO_MSG("Failled external predicate : ~p",[ExternalOntologyPredicate]),
          erlog_int:fail(ParentOntologyState)
      end;
    undefined ->
      erlog_int:fail(ParentOntologyState)
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

fail_external_predicate({_LCp, _LCps, _Lst}, Next0, ParentOntologyState, ExternalPredicate, ExternalState) ->
  %?INFO_MSG("Failling :~p   ~p    ~p",[{LCp, LCps, Lst}, Next0, ParentOntologyState]),
  case erlog_int:fail(ExternalState) of
    {succeed, NewExternalState} ->
      DDResultPredicate = erlog_int:dderef(ExternalPredicate,NewExternalState#est.bs),
      {succeed, NewBindings} = erlog_int:unify(DDResultPredicate, ExternalPredicate, ParentOntologyState#est.bs),
      erlog_int:prove_body(Next0, ParentOntologyState#est{bs = NewBindings});
    fail ->
      erlog_int:fail(ParentOntologyState)
  end.

%-------------------------------------------------------------------------------
%  Utilities
%-------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Convert a predicate string to its eterm representation
%% @end
%%------------------------------------------------------------------------------

-spec(string_to_eterm(list()) -> tuple).
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

prove(InitialPredicate, RootPredicateInitialKb, _Options) ->
  erlog_int:prove_goal(InitialPredicate, RootPredicateInitialKb).
