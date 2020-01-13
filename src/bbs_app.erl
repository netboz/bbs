%%%-------------------------------------------------------------------
%% @doc bbs public API
%% @end
%%%-------------------------------------------------------------------

-module(bbs_app).

-include("bbs.hrl").

-include("utils.hrl").

-behaviour(application).

-export([start/2, stop/1]).

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

    %
    _BubSpecs = #agent{name = <<"bubble">>, aid_entries = [],
		      startup_onts =
			     [
                {ontology, <<"bbs:bubble">>, []}
                ]},
               
    % Start application supervisor
    bbs_sup:start_link().

stop(_State) -> ok.

%% internal functions

