%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [
    pairs_key_value(Params, bubble_name(BubbleName)),
    pairs_key_value(Params, bubble_children(BubbleChildren)),
    goal(registered(BubbleName)),
    log(info,"Bubblename: ~p",[BubbleName]),
    initial_childs_started(BubbleChildren),
    "bbs:agent"::react_on(info_event, child_down(AnyChild, Reason), "bbs:bubble", 
        signal_process_exit(AnyChild, Reason), []),
    goal(terminated("test_bob"))],
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
    goal(child(Name, Ontologies, _Ref)),
    initial_childs_started(OtherChilds).


action(assert(bubble(BubbleName)), 
    [register_bubble(BubbleName),
    goal("bbs:agent"::stim_processed("bbs:bubble", bubble_registered(BubbleName)))
    ], 
    registered(BubbleName)).

% @doc: Start a nea child process running given agent specs
action(assert(child(Name, Ontologies, AgentRef)),[
    spawn_child(agent(Name, Ontologies), AgentRef),
    "bbs:agent"::goal(stim_processed("bbs:bubble", spawned(Name, Ontologies)))], 
    child(Name, Ontologies, AgentRef)).

% @doc: Stop child process ( process will terminate current goal and terminates )
action("bbs:agent"::goal(stim_processed("bbs:bubble", stopped(Name))), 
    [
        child(Name, Ontologies, AgentRef), 
        stop_child(AgentRef)], 
        stopped(Name)).

% @doc: Abruptly terminate child ( process stops any goal it was running and terminates )
action("bbs:agent"::goal(stim_processed("bbs:bubble", terminated(Name))), 
    [
        child(Name, Ontologies, AgentRef), 
        terminate_child(AgentRef)], 
            terminated(Name)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

signal_process_exit(Name, Reason) :-
    log(info,"Child  Exited : ~p   ~p", [Name, Reason]),
    retract(child(Name, Ontologies, Ref)),
    fail.

signal_process_exit(Name, Reason) :-
    "bbs:agent"::goal(stim_processed("bbs:bubble", exited(Name, Reason))).



