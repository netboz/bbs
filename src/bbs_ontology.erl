%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
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

%% API
-export([register_ontologies/1, get_registered_ont_desc/1, initialise_ontology/2]).
 
%% Predicates include into all ontologies that will be loaded
-define(BUILD_IN_PREDS,[]).

%%%%%%%%%%%%%%%%%%%%%%%% Ontologies registration


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

  case validate_built_in_preds(BuiltInPreds,[],[]) of
    {[],_ValidatedBIPs} ->
      ?INFO_MSG("~p :Validated all built in predicates.",[OntoNameSpace]);

    {UnvalidatedBIPS, _} ->
      lists:foreach(fun({Error, El}) -> ?ERROR_MSG("~p : ~p",[El, Error ]) end, UnvalidatedBIPS)
  end,
  ets:insert(?ONTO_STORE,{OntoNameSpace, TextPreds, BuiltInPreds}),
  register_ontologies(Others).



validate_built_in_preds([],Failled, Succeeded) ->
  {Failled, Succeeded};
validate_built_in_preds([Mod|OtherMods],Failled, Succeeded) ->
  case validate_built_in_pred(Mod) of
    ok ->
      validate_built_in_preds(OtherMods, Failled,Succeeded ++ [Mod]);
    Error ->
      validate_built_in_preds(OtherMods, Failled ++ [{Error,Mod}],Succeeded)
  end.

validate_built_in_pred(BuiltInPredMod) ->
  case code:ensure_loaded(BuiltInPredMod) of
    {module, _} ->
      ok;
    {error, Error} ->
      {error,Error}
  end.


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


get_registered_ont_desc(NameSpace) ->
  ets:lookup(?ONTO_STORE, NameSpace).

%%%%%%%%%%%%%%%%%  Initialisation of ontologies

initialise_ontology(Agent_name, {Ns,Params}) ->
  {ok, {Ns, Ns}}.




load_builded_in_preds(#est{} = Kb) ->
  lists:foldl(fun({Head, M, F}, Db) ->
    erlog_int:add_compiled_proc(Head, M, F, Db) end, Kb#est.db,
    ?BUILD_IN_PREDS).

%% This should allow list longer than one of builded in mods, but too lazy to do it right now
%% TODO: Manange list of built in preds mods
load_external_preds([PredMod|Others], Kb) ->
  load_external_preds(PredMod, Kb),
  load_external_preds(Others, Kb);

load_external_preds([], Kb) ->
  Kb;

load_external_preds(OntMod, Kb) when is_atom(OntMod) ->
  try OntMod:external_predicates() of
    Result when is_list(Result) ->
      lists:foldl(fun({Head, M, F}, Db) ->
        erlog_int:add_compiled_proc(Head, M, F, Db) end, Kb#est.db,
        Result),
      ok;
    _ -> ok
  catch
    Else ->
      ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [Else]),
      {error, Else};
    M:E ->
      ?ERROR_MSG("Error when fetching external predicates from ontology :~p", [{M, E}]),
      {error, {M, E}}
  end.


load_prolog_preds(TextPredList, #est{} = Kb) ->
  %% Return our Kb with all clauses loaded
  lists:foldl(fun(Entry, Est) -> load_predicate(Entry, Est) end, Kb, TextPredList).



load_predicate({file, File}, #est{} = Kb) ->
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
  ?INFO_MSG("Consulting : ~p", [filename:join([PrivDir, "prolog", File])]),


  try  erlog_file:consult(filename:join([PrivDir, "prolog", File]), Kb) of
    {ok, #est{} = NewEst} ->
      NewEst;
    Other ->
      ?WARNING_MSG("Unexpected result while loading predicates from file :~p", [{File, Other}]),
      Kb
  catch
    M:E ->
      ?INFO_MSG("ERROR loading predicates from file :~p ~p",[M,E])
  end;

load_predicate({string, Predicate}, DbRef) ->
  erlog_int:assertz_clause(string_to_eterm(Predicate), DbRef),
  ok;


load_predicate(Unk, DbSt) ->
  ?WARNING_MSG("Unrecognized intialisation predicate :~p", [Unk]),
  DbSt.


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