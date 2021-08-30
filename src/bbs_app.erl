%%%-------------------------------------------------------------------
%% @doc bbs public API
%% @end
%%%-------------------------------------------------------------------

-module(bbs_app).

-include("bbs.hrl").

-behaviour(application).

-export([start/2, stop/1, hostname/0]).
-export([new_bubble/1]).

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
  MotherBubChildSpecs =
    #agent{name = <<"root">>,
           parent = node(),
           startup_ontologies = StartUpOntologies},
  Root_bubble_specs =
    #{id => root_bubble,
      start => {bbs_agent, start_link, [MotherBubChildSpecs]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [bbs_agent_sup]},
  % Next effectively start root bubble
  {ok, _Pid} =
    ?SWARM:register_name({<<"root">>, node()},
                         supervisor,
                         start_child,
                         [AppSupPid, Root_bubble_specs]).

stop(_State) ->
  ok.

%% internal functions

default_root_bubble_ontologies() ->
  [{ontology, <<"bbs:brain_tests:data">>, [], bbs_db_ets},
   {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets},
   {ontology, <<"bbs:agent">>, [], bbs_db_ets},
   {ontology,
    <<"bbs:mts:client:gproc">>,
    [],
    bbs_db_ets}].    
    % {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
    % {ontology, <<"bbs:mts:mqtt:broker">>, [], bbs_db_ets},
    % {ontology, <<"bbs:mts:client:mqtt">>, [], bbs_db_ets},
    % {ontology, <<"bbs:root">>, [], bbs_db_ets}

%% API

new_bubble(#agent{} = Bubble_specs) ->
  gen_server:cast(bbs_lobby, {new_bubble, Bubble_specs}).

%% internal functions

%% Unit tests
hostname() ->
  case {inet_db:gethostname(), inet_db:res_option(domain)} of
    {H, D} when is_list(D), is_list(H), length(D) > 0, length(H) > 0 ->
      H ++ "." ++ D;
    Other ->
      error({hostname, Other})
  end.
