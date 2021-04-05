%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [
    %% Add hook to receive MQTT messages from mqtt client library
     "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, Message, Ts)),
        "bbs:agent", log(info,"Got MQTT :~p", [{Topic, Message}])),
    log(info,"1",[]),

    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:mqtt:client")),
    log(info,"2",[]),

    subscribe("testopic"),

    log(info,"3",[])

    ],
    initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).



send_message(Dest, Acc, Onto, Message) :-
    ok.


