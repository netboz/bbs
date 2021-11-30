%%%-------------------------------------------------------------------
%% @doc bbs public API
%% @end
%%%-------------------------------------------------------------------

-module(bbs_app).

-include("bbs.hrl").

-behaviour(application).

-export([start/2, stop/1, hostname/0]).
-export([new_bubble/1]).

start(_StartType, _StartArgs) ->
  register(bbs, self()),

  % Create ontologies index
  ets:new(?ONTO_STORE, [named_table, set, protected, {read_concurrency, true}]),

  %% Register ontologies
  Ontolist = application:get_env(bbs, ontologies, []),
  bbs_ontology:register_ontologies(Ontolist),

  Children =
    [{?HORDEREG, [{name, ?BBS_BUBBLES_REG}, {keys, unique}]},
     {?HORDESUP,
      [{name, ?BBS_BUBLES_SUP},
       {strategy, one_for_one},
       {distribution_strategy, ?HORDISTRIB},
       {max_restarts, 100_000},
       {max_seconds, 1}]}],

  Opts = [{strategy, one_for_one}, {name, bbs_app_sup}],
  'Elixir.Supervisor':start_link(Children, Opts),

  % {ok, _AppSupPid} = supervisor:start_link(Children, [{strategy, one_for_one}]),
  % % Supervisation specs for root bubble
  StartUpOntologies =
    application:get_env(bbs, root_bubble_ontologies, default_root_bubble_ontologies()),
  MotherBubChildSpecs =
    #agent{name = <<"root">>,
           parent = node(),
           startup_ontologies = StartUpOntologies},
  Root_bubble_specs =
    #{id => root_bubble,
      start => {bbs_agent, start_link, [MotherBubChildSpecs, uuid:get_v4_urandom()]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [bbs_agent_sup]},

  % % Next effectively start root bubble
  ?HORDESUP:start_child(?BBS_BUBLES_SUP, Root_bubble_specs).

  % {ok, _Pid} =
  %     ?HORDESUP:start_child(bbs_sup, Root_bubble_specs).

stop(_State) ->
  ok.

%% internal functions

default_root_bubble_ontologies() ->
  [{ontology, <<"bbs:brain_tests:data">>, [], bbs_db_ets},
   {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets},
   {ontology, <<"bbs:agent">>, [], bbs_db_ets},
   {ontology, <<"bbs:mts:client:registry">>, [], bbs_db_ets},
   {ontology,
    <<"bbs:bubble">>,
    [],
    bbs_db_ets}].   % {ontology, <<"bbs:bubble">>, [], bbs_db_ets},

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
