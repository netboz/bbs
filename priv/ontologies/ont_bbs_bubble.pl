%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [
    pairs_key_value(Params, bubble_name(BubbleName)),
    pairs_key_value(Params, bubble_children(BubbleChildren)),
    registered(BubbleName),
    initial_childs_started(BubbleChildren),
    "bbs:agent"::react_on(info_event, child_down(AnyChild, Reason), "bbs:bubble", 
        signal_process_exit(AnyChild, Reason), [])],
        initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).


pairs_key_value([Predicate|_], Pattern) :-
    Pattern = Predicate.
pairs_key_value([Predicate|OtherPredicates], Pattern) :-
    pairs_key_value(OtherPredicates, Pattern).
pairs_key_value(_, _).

initial_childs_started([]).
initial_childs_started([child(Name, Ontologies)|OtherChilds]) :-
    goal(child(Name, Ontologies)),
    initial_childs_started(OtherChilds).


action(assert(bubble(BubbleName)), 
    [register_bubble(BubbleName),
    "bbs:agent"::stim_processed("bbs:bubble", bubble_registered(BubbleName))
    ], 
    registered(BubbleName)).

% @doc: Start a nea child process running given agent specs
action(assert(child(Name, Ontologies)),[
    spawn_child(agent(Name, Ontologies)),
    "bbs:agent"::goal(stim_processed("bbs:bubble", spawned(Name, Ontologies)))], 
    child(Name, Ontologies)).

% @doc: Stop child process ( process will terminate current goal and terminates )
action( "bbs:agent":goal(stim_processed("bbs:bubble", stopped(Name, Ontologies))), 
    [child(Name, Ontologies), 
    stop_child(Name), 
    retract(Name, Ontologies)], 
        stopped(Name)).

% @doc: Abruptly terminate child ( process stops any goal it was running and terminates )
action("bbs:agent"::goal(stim_processed("bbs:bubble", terminated(Name))), 
    [child(Name, Ontologies), 
    terminate_child(Name)], 
        terminated(Name)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


signal_process_exit(Name, Reason) :-
    child(Name, Ontologies),
    retract(child(Name, Ontologies)),
    fail.

signal_process_exit(Name, Reason) :-
    "bbs:agent"::goal(stim_processed("bbs:bubble", exited(Name, Reason))).



