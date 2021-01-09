%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).

action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(spawn_agent(Name, Ontologies, Pid), [], agent(Name, _AIDs, Ontologies), Pid).
