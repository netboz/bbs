 -module(ont_bbs_agent_SUITE).

 -include_lib("common_test/include/ct.hrl").

 %% Test server callbacks
 -export([suite/0, all/0, 
	  init_per_suite/1, end_per_suite/1,
	  init_per_testcase/2, end_per_testcase/2]).

 