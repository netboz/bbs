%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc FSM describing a bbs agent
%%%
%%% @end
%%% Created : 10. Jan 2020 23:22
%%%-------------------------------------------------------------------
-module(bbs_agent).
-author("Yan").

-behaviour(gen_statem).

-include("bbs.hrl").
-include("utils.hrl").
-include_lib("erlog/include/erlog_int.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, ontologies_init/3, running/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%% Used to retrieve and store initial state for an ontology
-export([get_ontology_state_from_namespace/1, store_ontology_state_on_namespace/2]).

-define(SERVER, ?MODULE).

-record(state, {
  name,
  uuid,
  onto_to_init = [],
  onto_dict
}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(#agent{} = AgentSpecs) ->
  gen_statem:start_link(?MODULE, AgentSpecs, []).

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(AgentSpecs) ->
  ?INFO_MSG("Initialising Agent :~p   PID :~p",[AgentSpecs, self()]),
  Uuid = zuuid:binary(zuuid:v4()),
  {ok, ontologies_init, #state{name = AgentSpecs#agent.name,
    onto_dict = dict:new(),
    uuid = Uuid,
    onto_to_init = AgentSpecs#agent.startup_ontologies}, [{next_event, cast, init_next}]}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% First state after init, we starts by initialising the ontologies of the agent
%% @end
%%------------------------------------------------------------------------------

ontologies_init(enter, _, State) ->
  ?INFO_MSG("Ontology init :~p.",[State#state.name]),
  {keep_state,State};

ontologies_init(cast, init_next, State) when State#state.onto_to_init == [] ->
  %% Call the run predicate in each initialised ontologys
  ?INFO_MSG("All Ontologies initialised.",[]),
  {next_state,running,State};

ontologies_init(cast, init_next, #state{onto_to_init = [{ontology, Ns, Params, DbMod}|NextOntos]} = State) ->
  case bbs_ontology:build_ontology(State#state.uuid, Ns, DbMod) of
    {error, OntoBuildError} ->
      ?ERROR_MSG("~p Failled to initialize ontology :~p with error: ~p",[State#state.name, Ns, OntoBuildError]),
      {next_state, ontologies_init, State#state{onto_to_init = NextOntos}, [{next_event, cast, init_next}]};
    {ok, {_Tid, Kb}} ->
      ?INFO_MSG("Builded Knowledge Base for :~p",[Ns]),
      {succeed, KbReady} = erlog_int:prove_goal({set_prolog_flag, unknown, fail}, Kb),
      %% Register the raw kb state into process stack under key Namespace
      store_ontology_state_on_namespace(Ns, Kb),
      case prove(Ns, {goal,{initialized, State#state.uuid, Ns, Params}}) of
        {succeed, InitializedOntoState} ->
          {_K, KbReady} = erlog_int:prove_goal({set_prolog_flag, unknown, fail}, InitializedOntoState),
          {ok, {Ns, KbReady}};
        {{paused, Ns, PredPatFunc, PredPatParams}, Next0, St} ->
           ontolog:put_onto_hooks({Ns, PredPatFunc}, {PredPatParams, Next0, St},[once]),
          {{paused, Ns, PredPatFunc, PredPatParams}, Next0, St};
        {fail, FStatr} ->
          ?ERROR_MSG("Ontology initialisation failed :~p with state: ~p", [Ns, FStatr]),
          {error, preinit_failled};
        Else ->
          ?ERROR_MSG("Ontology unexpected result :~p", [Else])
      end,

      {next_state, ontologies_init, State#state{onto_to_init = NextOntos}, [{next_event, cast, init_next}]}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Ontologies are initialized, we now sequentially call ```boot```
%% predicate on all of them
%% @end
%%------------------------------------------------------------------------------

running(enter, _, State) ->
  ?INFO_MSG("Agent ~p is running.",[State#state.name]),
  {keep_state,State};

running(cast, {NameSpace, Fact}, State) ->
  ?INFO_MSG("Agent ~p checking for stims on :~p",[State#state.name, {NameSpace,Fact}]),
  trigger_stims(NameSpace,Fact),
  {next_state, running, State};

running(UnkMesTyp, UnkMes, State) ->
  ?INFO_MSG("Agent ~p Received unmanaged state messsage :~p",[State#state.name, {UnkMesTyp,UnkMes}]),
  {next_state, running, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #state{}, _Extra) ->
  {ok, StateName, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Prove predicate onto Namespace ontology
%%
%% @end
%%------------------------------------------------------------------------------

prove(NameSpace, Predicate) ->
  prove(NameSpace, Predicate, []).

prove(NameSpace, Predicate, Options) ->
  %TODO: next instruction fail, should mimic the failling of a prolog predicate
  OntologyInitialState = get_ontology_state_from_namespace(NameSpace),
  bbs_ontology:prove(Predicate, OntologyInitialState, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%%  Get an raw ontology state stored under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec(get_ontology_state_from_namespace(Ns ::binary()) -> undefined | tuple()).
get_ontology_state_from_namespace(Ns) ->
  get(Ns).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Store an ontology state under a Namespace
%% @end
%%------------------------------------------------------------------------------

-spec(store_ontology_state_on_namespace(Ns ::binary(), Kb :: term()) -> undefined | term()).
store_ontology_state_on_namespace(Ns, KbDb) ->
  put(Ns, KbDb).

%%-------------------------------------------------------------------------------
%% @doc
%% @private
%%  check all hooks stored for Namespace:Predicate and run the ones whose
%%  parameters can be unified.
%% @end
%%------------------------------------------------------------------------------

trigger_stims(NameSpace, PredicateIn) ->
  case get_triggers_predicate(NameSpace, PredicateIn) of
    %%We have a found a st
    [_FirstCandidate|_OtherCandidates] = ListOfReactionCandidates ->
      %% Functor of predicate
      FactFuncIn = element(1,PredicateIn),
      %%?INFO_MSG("Stm list :~p",[ListOfCandidatesReaction]),
      FList = [{El,PredicateIn} || El <- ListOfReactionCandidates],
      %%?INFO_MSG("FList :~p",[lists:nth(1,FList)]),
      Result = lists:foldl(fun filter_stim/2, {[],[]}, FList),
      %%?INFO_MSG("Stored stims :~p",[Result]),
      {DiscardedPreds, StimToBeProcessed} = Result,
      ?INFO_MSG("Stim to be processsed :~p",[StimToBeProcessed]),
      %%?INFO_MSG("Stim discarded : ~p",[DiscardedPreds]),
      KeptAfterProcessingPreds = lists:filtermap(fun process_stim/1,StimToBeProcessed),
      %%?INFO_MSG("Kept after processing :~p",[KeptAfterProcessingPreds]),
      FinalNewHooks = DiscardedPreds ++ KeptAfterProcessingPreds,
      case FinalNewHooks of
        [] -> ?INFO_MSG("Erasing :~p",[{reaction, iolist_to_binary(NameSpace),FactFuncIn}]),
          erase({reaction, iolist_to_binary(NameSpace),FactFuncIn});
        _ ->  put({reaction, iolist_to_binary(NameSpace),FactFuncIn}, DiscardedPreds ++ KeptAfterProcessingPreds)
      end;

    [] ->
      ?INFO_MSG("No reaction to ~p:~p found",[NameSpace,PredicateIn]),
      ok
  end.


get_triggers_predicate(NameSpace, Predicate) ->
  %% Get predicate functor
  InputPredicate = element(1,Predicate),
  %% Right now triggers are stored into process state.
  %% todo: store triggers somewhere else
  case get({reaction, iolist_to_binary(NameSpace), InputPredicate}) of
    undefined -> [];
    ListOfPredicate -> ListOfPredicate
  end.



unify_action_hook(PredicateIn, PredPattern) ->
  case erlog_int:unify_head(PredicateIn, PredPattern, dict:new(), 1) of
    fail -> false;
    {succeed, Bindings, _State, _Vn} -> {succeed, Bindings}
  end.

filter_stim({{PredPatParams, Next0, St, Options}, StimPattern},{NotMatchingPreds, MatchingPreds}) when is_record(St, est)->
  StimPatternFunctor = element(1, StimPattern),
  %%?INFO_MSG("Unifying : ~p      ~p",[StimPattern, list_to_tuple([StimPatternFunctor] ++ PredPatParams)]),
  case unify_action_hook(StimPattern, list_to_tuple([StimPatternFunctor] ++ PredPatParams)) of
    {succeed, Bindings} ->
      {NotMatchingPreds, MatchingPreds ++ [{{StimPatternFunctor, PredPatParams}, Next0, St, Options, Bindings}]};
    _ ->
      {NotMatchingPreds ++ [{PredPatParams, Next0, St, Options}], MatchingPreds}
  end;


filter_stim({{StimReactArgs, Reaction_ont, ReactionPred, Options}, StimPattern}, {NotMatchingPreds, MatchingPreds}) ->
  StimPatternFunctor = element(1, StimPattern),
  case unify_action_hook(StimPattern, list_to_tuple([StimPatternFunctor] ++ StimReactArgs)) of
    {succeed, Bindings} ->
      {NotMatchingPreds, MatchingPreds ++ [{{StimPatternFunctor, StimReactArgs}, Reaction_ont, ReactionPred, Options, Bindings}]};
    _ ->
      {NotMatchingPreds ++ [{StimReactArgs, Reaction_ont, ReactionPred, Options}], MatchingPreds}
  end;

filter_stim(A,B) ->
  ?WARNING_MSG("Unmanaged stim :~p",[A]),
  B.



process_stim({{PredFunc,PredParams}, Next0, #est{} = St, Options, Bindings}) ->
  %% TODO: change next function call to a API into module ontology

%%  BindingsAsDict = dict:from_list(Bindings),
%%  FunMdrge = fun(Key, Value1, Value2) -> ?INFO_MSG("Conflict Merging Dict :~p",[{Key, Value1, Value2}]) end,
%%  NewBindings = dict:merge(FunMdrge, St#est.bs, BindingsAsDict),

  %%?INFO_MSG("--> Back wait for : Next0 ~p  ~n CPS :~p",[Next0,St#est.cps]),
  %%?INFO_MSG("Stored DB :~p",[St#est.db]),
  case ontology:prove(Next0, St, Bindings) of
    {{paused, Ns, PredPatFunc,PredPatParams}, Next2, St2} ->
      %% TODO: This is bad, Db.ref should be considered as blackbox
      {CurrentNs,_DbKey} = St#est.db#db.ref,
      %%?INFO_MSG("Ns : ~p    CurrentNs :~p",[Ns,CurrentNs]),
      if
        (Ns == CurrentNs) andalso (PredPatFunc == PredFunc) ->
          case lists:member(once,Options) of
            true ->
              %%?INFO_MSG("Deleting hook :~p",[PredParams]),
              %% We replace the hook to be deleted with the new one
              {true, {PredPatParams, Next2, St2, [once]}};
            _ ->
              %% We need to keep botth hooks
              %% return a list of previous hook + new one, flattenned later on :(
              {true, [{PredParams, Next0, St, Options},{PredPatParams, Next2, St2, [once]}]}
          end;
      %%Add to current hooks;
        true ->
          %% Hook into another NS, simply add it
          ontology:put_onto_hooks({iolist_to_binary([Ns]), PredPatFunc}, {PredPatParams, Next2, St2},[once]),
          case lists:member(once,Options) of
            true ->
              %%?INFO_MSG("Deleting hook :~p",[PredParams]),
              false;
            _ ->
              {true, {PredParams, Next0, St, Options}}
          end
      end ;

    _E ->
      case lists:member(once,Options) of
        true ->
          %%?INFO_MSG("Deleting hook :~p",[PredParams]),
          false;
        _ ->
          {true, {PredParams, Next0, St, Options}}
      end
  end;


process_stim({{PredFunc,PredParams}, Reaction_ont, ReactionPred, Options, Bindings}) ->
  %%?INFO_MSG("Reacting to ~p ~p with ~p  ~p",[PredFunc,PredParams,Reaction_ont, ReactionPred]),
  %% case ontology:prove(Reaction_ont, ReactionPred, dict:from_list(Bindings)) of
  %%AgKey = gproc:get_value({p, l, {aid_entries, name}}),

  %%{ok, ErlState} = erlog_int:new(erlog_db_bbs, {AgKey,Reaction_ont}),

  %%?INFO_MSG("Bindings :~p",[Bindings]),
  %%?INFO_MSG("rection pred  :~p",[ReactionPred]),

  case ontology:prove(Reaction_ont, ReactionPred, Bindings) of
    {{paused, Ns, PredPatFunc, PredPatParams}, Next0, St} ->
      {_AgName,CurrentNs} = St#est.db#db.ref,
      if
        (Ns == CurrentNs) andalso (PredPatFunc == PredFunc) ->
          case lists:member(once,Options) of
            true ->
              %% We replace current hook with new one
              {true, {PredPatParams, Next0, St, [once]}};
            _ ->
              %% We need to keep both hooks.
              {true, [{PredParams, Reaction_ont, ReactionPred, Options},  {PredPatParams, Next0, St, [once]}]}
          end;
        true ->
          ontology:put_onto_hooks({Ns, PredPatFunc}, {PredPatParams, Next0, St},[once]),
          case lists:member(once,Options) of
            true ->
              %%?INFO_MSG("Deleting hook",[]),
              false;
            _ ->
              {true, {PredParams, Reaction_ont, ReactionPred, Options}}
          end
      end;

    _E->
      %%?INFO_MSG("Process stimm result :~p",[_E]),
      case lists:member(once,Options) of
        true ->
          %%?INFO_MSG("Deleting hook",[]),
          false;
        _ ->
          {true, {PredParams, Reaction_ont, ReactionPred, Options}}
      end

  end.

