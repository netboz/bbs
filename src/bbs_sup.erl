%%%-------------------------------------------------------------------
%% @doc bbs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bbs_sup).
-include("bbs.hrl").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags =
    #{strategy => one_for_all,
      intensity => 10,
      period => 1},

  StartUpOntologies = application:get_env(bbs, root_bubble_ontologies, default_root_bubble_ontologies()),

  MotherBubChildSpecs =
    #agent{name = <<"root">>,
      startup_ontologies = StartUpOntologies
    },

  AgentsupervisorSpecs = #{id => bubbles_sup,
    start => {bbs_agent_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [bbs_agent_sup]},

  Root_bubble_specs = #{id => root_bubble,
    start => {bbs_agent, start_link, [MotherBubChildSpecs]},
    restart => permanent,
    shutdown => infinity,
    type => worker,
    modules => [bbs_agent_sup]},

  {ok, {SupFlags, [AgentsupervisorSpecs, Root_bubble_specs]}}.

%% internal functions

default_root_bubble_ontologies() ->
  [
    % broken {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets},
    {ontology, <<"bbs:agent">>, [], bbs_db_ets},
    {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
    {ontology, <<"bbs:mts:mqtt:broker">>, [], bbs_db_ets},
    {ontology, <<"bbs:mts:mqtt:client">>, [], bbs_db_ets},
    {ontology, <<"bbs:root">>, [], bbs_db_ets}
  ].