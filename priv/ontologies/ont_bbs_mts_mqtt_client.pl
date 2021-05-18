%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [
    %% Add hook to receive MQTT messages from mqtt client library
    "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, Payload, Ts)),
        "bbs:mts:mqtt:client", process_incoming_mqtt_message(Id, Ts, From, To, Topic, Payload)),
        "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, Payload, Ts)),
                "bbs:mts:mqtt:client", process_incoming_mqtt_message(Id, Ts, From, To, Topic, Payload)),

    goal(cc(Me, "test", "bbs:agent")),
    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:mqtt:client"))
    ],
    initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
     assert(me(AgentId)),
    assert(initialized(Ns, Ag, Params)).

%% As agent subscribed to the topic, it receive a copy of the message it posts. This is to igonre them.
process_incoming_mqtt_message(Id, Ts, From, To, Topic, Payload) :-
    me(From).

process_incoming_mqtt_message(Id, Ts, From, To, Topic, Payload) :-
    type_of(Payload, predicate),
    log(info,"Incoming mqtt message :~p",[Payload]),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, Payload)).

process_incoming_mqtt_message(Id, Ts, From, To, Topic, Payload) :-
    type_of(Payload, string),
    log(info,"Incoming mqtt prolog message :~p",[Payload]),
    "bbs:agent"::prolog_interpretation(Payload, MessagePrologTerms),
     log(info,"Converted :~p",[MessagePrologTerms]),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, MessagePrologTerms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(assert(cc(Me, CcId, Ontology)), ["bbs:agent"::me(Me), \+cc(_, CcId, _), new_cc(CcId, Ontology)], cc(Me, CcId, Ontology)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(mqtt_send(CcId, To, Ontology, Predicate), [], sent(CcId, message(To, Ontology, Predicate))).





