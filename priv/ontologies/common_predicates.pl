%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions processing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(Goal) :-
    call(Goal).

goal(Goal) :-
     log(info, "Looking to satisfy Goal : ~p", [Goal]),
     action(Action, Prereq, Goal),
     satisfy_prereq(Prereq),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).
