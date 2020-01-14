%%%-------------------------------------------------------------------
%% @doc bbs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bbs_agent_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, bubbles_sup).

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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpecs =  [{bbs_agent, {bbs_agent, start_link, []},
        transient, 2000, worker, [bbs_agent]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
