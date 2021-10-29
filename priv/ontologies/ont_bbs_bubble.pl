%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params), [
    registered(BubbleName),
    "bbs:agent"::react_on(info_event, child_down(AnyChild, Reason), "bbs:bubble", signal_process_exit(AnyChild, Reason),[]),
    child("test_bob",[
        ontology("bbs:agent", [], bbs_db_ets), 
        ontology("bbs:mts:client:registry", [], bbs_db_ets),
        ontology("bbs:mts:client:mqtt", [], bbs_db_ets)])
    ],
        initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).


action(assert(child_bubble(BubbleName)), [register_bubble(BubbleName)], registered(BubbleName)).

% @doc: Start a nea child process running given agent specs
action(assert(child(Name, Ontologies)),[spawn_child(agent(Name, Ontologies))], child(Name, Ontologies)).

% @doc: Stop child process ( process will terminate current goal and terminates )
action(stop_child(Name), [], stopped(Name)).

% @doc: Abruptly terminate child ( process stops any goal it was running and terminates )
action(terminate_child(Name), [], terminated(Name)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


signal_process_exit(Name, Reason) :-
    child(Name, Ontologies),
    retract(child(Name, Ontologies)).




