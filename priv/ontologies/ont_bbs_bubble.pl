%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).
initialize(Ns, Ag, Params) :-
    log(info,"Bubble ontology initialized",[]),
    assert(initialized(Ns, Ag, Params)).



action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    log(info,"Bubble entering itself",[]),
    goal(inside_bubble(Ag)),
    assert(booted(Ns, Ag, Params)).

