%%%-------------------------------------------------------------------
%% @doc bbs public API
%% @end
%%%-------------------------------------------------------------------

-module(bbs_app).

-include("bbs.hrl").
-include("utils.hrl").
-behaviour(application).

-export([start/2, stop/1]).
-export([new_bubble/1]).
%-export([get_node/1, get_parent/1, get_tree/1, dump_tree/0]).

start(_StartType, _StartArgs) ->
    register(bbs, self()),

    {AgentsBackendMod, AgentsBackendModInitParameters} = application:get_env(
        bbs, registry_module, {bbs_registry_lasp, #{}}
    ),
    persistent_term:put(agent_backend_mod, AgentsBackendMod),

    case bbs_agents_backend:init(AgentsBackendModInitParameters) of
        ok ->
            %% Seed random number generator
            quickrand:seed(),

            % Create ontologies index
            ets:new(?ONTO_STORE, [named_table, set, protected, {read_concurrency, true}]),

            %% Register ontologies
            Ontolist = application:get_env(bbs, ontologies, []),
            bbs_ontology:register_ontologies(Ontolist),

            RootGrain = erleans:get_grain(
                bbs_statefull_agent_grain, {<<"root">>, <<"root_node">>, null}
            ),

            bbs_statefull_agent_grain:new_ontology(RootGrain, default_root_bubble_ontologies()),

            ?INFO_MSG("Started Rootgrain ~p", [RootGrain]),
            
            {ok, self()};
        {error, Reason} ->
            ?ERROR_MSG("Couldn't initialise agents backend : ~p", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    ok.

%% internal functions

default_root_bubble_ontologies() ->
    [
        {ontology, <<"bbs:agent">>, [], bbs_db_ets},
        %{ontology, <<"bbs:mts:client:registry">>, [], bbs_db_ets},
        {ontology, <<"bbs:bubble">>,
            [
                {bubble_children, [
                    {child, <<"test_bob">>, [
                        {ontology, <<"bbs:agent">>, [], bbs_db_ets},
                        % {ontology, <<"bbs:mts:client:registry">>, [], bbs_db_ets},
                        {ontology, <<"bbs:mts:client:mqtt">>,
                            [
                                {clients, [
                                    {connection, <<"localhost">>, 1883, <<"root">>, [], [
                                        {subscription, <<"test_bob">>}
                                    ]}
                                ]}
                            ],
                            bbs_db_ets}
                    ]}
                ]}
            ],
            bbs_db_ets}
    ].
%?HORDESUP:start_child(?BBS_BUBLES_SUP, Root_bubble_specs)
%% API

new_bubble(#agent{} = BubbleSpecs) ->
    gen_server:cast(bbs_lobby, {new_bubble, BubbleSpecs}).
