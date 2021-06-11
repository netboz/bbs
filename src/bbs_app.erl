%%%-------------------------------------------------------------------
%% @doc bbs public API
%% @end
%%%-------------------------------------------------------------------

-module(bbs_app).

-include("bbs.hrl").
-include("utils.hrl").

-behaviour(application).

-export([start/2, stop/1]).
-export([new_bubble/1, new_bubble_test/0]).

-define(SWARM, 'Elixir.Swarm').

start(_StartType, _StartArgs) ->
  register(bbs, self()),
  application:ensure_all_started(swarm),
  % Create ontologies index
  ets:new(?ONTO_STORE, [named_table, set, protected, {read_concurrency, true}]),

  %% Register ontologies
  Ontolist = application:get_env(bbs, ontologies, []),
  bbs_ontology:register_ontologies(Ontolist),

  % Start root bubble supervisor
  {ok, AppSupPid} = bbs_sup:start_link(),

  % Supervisation specs for root bubble
  StartUpOntologies =
    application:get_env(bbs, root_bubble_ontologies, default_root_bubble_ontologies()),
  MotherBubChildSpecs = #agent{name = <<"root">>, parent = node(), startup_ontologies = StartUpOntologies},
  Root_bubble_specs =
    #{id => root_bubble,
      start => {bbs_agent, start_link, [MotherBubChildSpecs]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [bbs_agent_sup]},
  % Next effectively start root bubble
  {ok, _Pid} = ?SWARM:register_name({<<"root">>, node()}, supervisor, start_child, [AppSupPid, Root_bubble_specs]).

stop(_State) ->
  ok.

%% internal functions

default_root_bubble_ontologies() ->
  [{ontology, <<"bbs:brain_tests:data">>, [], bbs_db_ets},
    {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets},
    {ontology, <<"bbs:agent">>, [], bbs_db_ets},
    {ontology, <<"bbs:mts:client:gproc">>, [], bbs_db_ets},
    {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
    %   {ontology, <<"bbs:mts:mqtt:broker">>, [], bbs_db_ets},
    %   {ontology, <<"bbs:mts:mqtt:client">>, [], bbs_db_ets},
    {ontology, <<"bbs:root">>, [], bbs_db_ets}].

%% API

new_bubble(#agent{} = Bubble_specs) ->
  gen_server:cast(bbs_lobby, {new_bubble, Bubble_specs}).

%% internal functions

%% Unit tests

new_bubble_test() ->
  Id = base64:encode(crypto:strong_rand_bytes(8)),
  BubSpecs =
    #agent{name = Id,
      startup_ontologies =
      [% broken {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets},
        {ontology, <<"bbs:agent">>, [], bbs_db_ets},
        %    {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
            {ontology, <<"bbs:mts:client:gproc">>, [], bbs_db_ets}
        %    {ontology, <<"bbs:mts:mqtt:client">>, [], bbs_db_ets},
        %    {ontology, <<"bbs:root">>, [], bbs_db_ets}]},
      ]},
  % Supervisation specs for root bubble

  Root_bubble_specs =
    #{ id => Id,
      start => {bbs_agent, start_link, [BubSpecs]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [bbs_agent_sup]},

  {ok, _Pid} =  ?SWARM:register_name(Id, supervisor, start_child, [bbs_sup, Root_bubble_specs]).

