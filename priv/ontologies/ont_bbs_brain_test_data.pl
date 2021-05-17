%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).


external_predicate(first_solution).
external_predicate(second_solution).

valid_external_solution(second_solution).

faillure(bob).
faillure(sam).
faillure(antoine).
faillure(patrick).


