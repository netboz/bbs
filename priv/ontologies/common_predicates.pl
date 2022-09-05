%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions processing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(NameSpace::Goal) :-
     NameSpace::goal(Goal).

goal(Goal) :-
     log(info, "Goal : ~p", [Goal]),
    call(Goal).

goal(Goal) :-
     action(Action, Prereq, Goal),
     satisfy_prereq(Prereq),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     call(Goal1),
     satisfy_prereq(Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing functionnality %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(assert(tested(Ontology)), [me(AgentId), findall(Test, Ontology::test(Test,AgentId), Results)], tested(Ontology)).



