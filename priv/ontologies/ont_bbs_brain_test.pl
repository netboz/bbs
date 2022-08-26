%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [
    log(info,"Starting brain test",[]),
    "bbs:brain_tests:data"::external_predicate(Value),
    log(debug,"Solution :~p",[Value]),
    "bbs:brain_tests:data"::valid_external_solution(Value),
    log(debug," Good Solution :~p",[Value]),
    \+"bbs:brain_tests:data"::faillure(tom),
    log(info,"Finished tests",[])
    ], initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).

