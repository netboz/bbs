%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2020 23:22
%%%-------------------------------------------------------------------
-module(bbs_agent).
-author("Yan").

-behaviour(gen_statem).

-include("bbs.hrl").
-include("utils.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([init/1, ontologies_init/3, running/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(state, {
  name,
  uuid,
  onto_to_init = [],
  onto_dict
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(#agent{} = AgentSpecs) ->
  gen_statem:start_link(?MODULE, AgentSpecs, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(AgentSpecs) ->
  ?INFO_MSG("Initialising Agent :~p",[AgentSpecs]),
  Uuid = uuid:uuid4(),
  {ok, ontologies_init, #state{name = AgentSpecs#agent.name, 
    onto_dict = dict:new(),
    uuid = Uuid, 
    onto_to_init = AgentSpecs#agent.startup_onts}, [{next_event, cast, init_next}]}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

ontologies_init(enter, _, State) ->
  ?INFO_MSG("Ontology init :~p.",[State#state.name]),
  {keep_state,State};

ontologies_init(cast, init_next, State) when State#state.onto_to_init == [] ->
  %% Call the run predicate in each initialised ontology
  ?INFO_MSG("All Ontologies initialised.",[]),
  {next_state,running,State};

ontologies_init(cast, init_next, #state{onto_to_init = [{ontology,Ns,Params}|NextOntos]} = State) ->
  case bbs_ontology:initialise_ontology(State#state.name, {Ns,Params}) of
    {error, preinit_failled} ->
      ?ERROR_MSG("~p Failled to boot :~p",[State#state.name,{Ns,Params}]),
      {next_state, ontologies_init, State#state{onto_to_init = NextOntos}, [{next_event, cast, init_next}]};
      %%{stop, failled_boot};

    {ok, {_Ns, _OntRec}} ->
      ?INFO_MSG("Agent ~p initialised ontology : ~p",[State#state.name, Ns]),
      {next_state, ontologies_init, State#state{onto_to_init = NextOntos}, [{next_event, cast, init_next}]}
end.

running(enter, _, State) ->
  ?INFO_MSG("Agent ~p is running.",[State#state.name]),
  {keep_state,State}.



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

%%%===================================================================
%%% Internal functions
%%%===================================================================
