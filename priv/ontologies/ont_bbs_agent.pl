%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% goal       :initialized(+Ns, +Ag, +Params)
%%              Initialize this ontology

action(initialize(Ns, Ag, Params), [new_knowledge_base("bbs:agent:stims")], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    log(info,"Waking up agent :~p",[AgentId]),
    assert(initialized(Ns, Ag, Params)).

action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    assert(booted(Ns, Ag, Params)).

%% goal       :stim_processed(+NamespaceIn, +PredicateIn)
%%              execute all stims associated with NamespaceIn::PredicateIn

action(process_stims(NamespaceIn, PredicateIn),
    [],
    stim_processed(NamespaceIn, PredicateIn)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Stim related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% predicate        : react_on(+NsIn, +PredIn, +NsOut, +PredOut)
%%                  Ask agent to react to NsIn::PredIn with NsOut::PredOut

react_on(NsIn, PredIn, NsOut, PredOut) :-
    react_on(NsIn, PredIn, NsOut, PredOut, []).

react_on(NsIn, PredIn, NsOut, PredOut, Options) :-
        "bbs:agent:stims"::assert(( stim(NsIn, PredIn, Options) :-
            NsOut::PredOut )).

%% predicate        : process_stims(+NsIn, +PredIn)
%%                  Effectively execute stims and delete needed ones

process_stims(NamespaceIn, PredicateIn) :-
    \+execute_and_delete_stim(NamespaceIn, PredicateIn).

execute_and_delete_stim(NamespaceIn, PredicateIn) :-
    Head = stim(NamespaceIn, PredicateIn, Options),
    "bbs:agent:stims"::clause(Head, Body),
     "bbs:agent:stims"::Head,
    member(once, Options),
    "bbs:agent:stims"::retract(( Head :- Body )),
    fail.




