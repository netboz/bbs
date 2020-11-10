%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [log(info,"Starting brain test",[]), "bbs:brain_tests"::external_predicate(Value), log(debug,"Solution :~p",[Value]), valid_external_solution(Value), log(debug," Good Solution :~p",[Value])], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).


action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    assert(booted(Ns, Ag, Params)).


external_predicate(first_solution).
external_predicate(second_solution).

valid_external_solution(second_solution).


goal(Goal) :-
    %%log(info, "Checking if  ~p",[Goal]),
    call(Goal).

goal(Goal) :-
     log(info, "Looking to satisfy Goal : ~p", [Goal]),
     action(Action, Prereq, Goal),
     %%log(info, "prereqs ~p",[Prereq]),
     satisfy_prereq(Prereq),
    log(debug,"Prereqs ok, performing action :~p",[{Onto,Action}]),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).


