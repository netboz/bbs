%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    log(info,"Waking up agent :~p",[AgentId]),
    assert(initialized(Ns, Ag, Params)).

action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    assert(booted(Ns, Ag, Params)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(Goal) :-
    call(Goal).

goal(Goal) :-
     log(info, "Looking to satisfy Goal : ~p", [Goal]),
     action(Action, Prereq, Goal),
     %%log(debug, "Next action :~p",[Action]),
     %%log(debug, "prereqs ~p",[Prereq]),
     satisfy_prereq(Prereq),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).
