%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    ["bbs:agent":parent_bubble(ParentBubble), register_gproc(ParentBubble)],
        initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).

