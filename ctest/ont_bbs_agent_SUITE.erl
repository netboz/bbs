
-module(ont_bbs_agent_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("erlog/include/erlog_int.hrl").

%% Test server callbacks
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1,
         end_per_suite/1]).
-export([mocked_agent/2]).

all() ->
  [].

init_per_suite(Configdata) ->
  application:ensure_all_started(bbs),
  Configdata.

end_per_suite(_Configdata) ->
  ok.

init_per_testcase(_Test, Configdata) ->
  Configdata.

end_per_testcase(_Test, Configdata) ->
  Configdata.





mocked_agent(AgentName, ParentName) ->
  gproc:reg({n, g, {AgentName, ParentName}}, unused),
  receive
    _ ->
      ok
  after 3000 ->
    ok
  end.
