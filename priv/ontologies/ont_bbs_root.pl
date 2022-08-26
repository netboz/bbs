%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [], initialized(AgentId, Parent, NameSpace, Params)).

initialize(AAgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).
