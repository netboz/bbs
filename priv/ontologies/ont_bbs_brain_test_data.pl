%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [], initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).


external_predicate(first_solution).
external_predicate(second_solution).

valid_external_solution(second_solution).

faillure(bob).
faillure(sam).
faillure(antoine).
faillure(patrick).


