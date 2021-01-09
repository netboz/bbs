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

start(_StartType, _StartArgs) ->
  register(bbs, self()),
  % Create ontologies index
  ets:new(?ONTO_STORE,
    [named_table, set, protected,
      {read_concurrency, true}]),
  %% Register ontologies
  Ontolist = case application:get_env(bbs, ontologies) of
               undefined ->
                 ?WARNING_MSG("No ontology registered", []), [];
               {ok, Value} -> Value
             end,
  bbs_ontology:register_ontologies(Ontolist),


  % Start application supervisor
  bbs_sup:start_link().

stop(_State) -> ok.

%% API

new_bubble(#agent{} = Bubble_specs) ->
  supervisor:start_child(bubbles_sup, [Bubble_specs]).


%% internal functions


%% Unit tests

new_bubble_test() ->
  BubSpecs = #agent{name = <<"bubble">>, aid_entries = [],
    startup_ontologies =
    [
      {ontology, <<"bbs:brain_tests">>, [], bbs_db_ets}
     % {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
     % {ontology, <<"bbs:agent">>, [], bbs_db_ets}
    ]},
  new_bubble(BubSpecs).
