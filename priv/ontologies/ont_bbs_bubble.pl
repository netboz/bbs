%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params), [child("test_bob",[]),
    "bbs:agent"::react_on(info_event, 'EXIT'(Pid, Reason), "bbs:bubble", signal_process_exit(Pid, Reason),[once]),

    stopped("test_bob")], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).

% @doc: Start a nea child process running given agent specs
action(assert(child(Name, Ontologies)),[spawn_child(agent(Name, Ontologies), Pid), assert(process(Name, Pid))], child(Name, Ontologies)).

% @doc: Stop child process ( process will terminate current goal and terminates )
action(stop_child(Pid), [process(Name, Pid)], stopped(Name)).

% @doc: Abruptly terminate child ( process stops any goal it was running and terminates )
action(terminate_child(Pid), [process(Name, Pid)], terminated(Name)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

signal_process_exit(Pid, Reason) :-
    log(info,"----->1",[]),

    process(Name, Pid),
    child(Name, Ontologies),
    retract(child(Name, Ontologies)).


