%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).

action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    log(info,"Bubble entering itself",[]),
    goal(inside_bubble(Ag)),
    assert(booted(Ns, Ag, Params)).



goal(Goal) :-
    log(debug, "Checking if  ~p",[Goal]),
    call(Goal).

goal(Goal) :-
     log(info, "Looking to satisfy Goal: ~p", [Goal]),
     action(Action, Prereq, Goal),
     log(info, "prereqs ~p",[Prereq]),
     satisfy_prereq(Prereq),
     log(debug,"Prereqs ok performing action :~p",[{Onto,Action}]),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).


