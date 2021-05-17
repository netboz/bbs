%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [
    log(info,"Starting brain test",[]),
    "bbs:brain_tests:data"::external_predicate(Value),
    log(debug,"Solution :~p",[Value]),
    "bbs:brain_tests:data"::valid_external_solution(Value),
    log(debug," Good Solution :~p",[Value]),
    \+"bbs:brain_tests:data"::faillure(tom),
    log(info,"Finished tests",[])
    ], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).


action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    assert(booted(Ns, Ag, Params)).

