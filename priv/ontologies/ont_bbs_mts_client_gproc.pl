%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params),
    [
    assert(me(AgentId)),
    assert(parent(Parent)),
    %% Add hook to receive MQTT messages from mqtt client library
    "bbs:agent"::react_on(info_event, incoming_gproc(Topic, Payload),
        "bbs:agent", goal(incoming_data_processed(Topic, Payload))),


    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:gproc"))
    ],
    initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
     "bbs:agent:ccs"::assert(( cc("bbs:mts:client:gproc", Ontology, Parent) :-
                                    "bbs:mts:mqtt:client"::cc(CCId, To, Ontology, Parent))),
    assert(initialized(AgentId, Parent, NameSpace, Params)).



goal(join_cc_predicate(CcId, Ontology),[\+"bbs:agent:ccs"::cc(CcId, _, _, _)], cc(CcId, Ontology)).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(send(CcId, Message), [], sent(CcId, Message)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Tests  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(test_join, AgentId) :-
     log(info, "Running test: ~p", [test_join]),
     log(info,"1",[]),
     join_cc("test_cc", "bbs:agent"),
          log(info,"2",[]),

     "bbs:agent"::react_on_stim("bbs:agent", incoming_data(_, _), "bbs:agent", log(info,"BOBOBOBOBOBO", []), [once]),
          log(info,"3",[]),

     goal(sent("test_cc", test_atom)),
          log(info,"4",[]),

     test_join(test_atom),
     log(info, "Test test_join succeeded ").



