%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [
    %% Add hook to receive MQTT messages from mqtt client library
    "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, incoming_mqtt_message(To, Ont, Pred), Ts)),
        "bbs:mts:mqtt:client", process_incoming_mqtt_message(Id, Ts, Topic, incoming_mqtt_message(From, To, Ont, Pred))),

    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:mqtt:client"))
    ],
    initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).


process_incoming_mqtt_message(Id, Ts, Topic, incoming_mqtt_message(From, To, Ontology, Predicate)) :-
    log(info,"Incoming mqtt message :~p",[Predicate]),
    "bbs:agent"::goal(incoming_message_processed(Topic, message(From, To, Ontology, Predicate))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(assert(cc(Me, CcId, Ontology)), ["bbs:agent"::me(Me), \+cc(_, CcId, _), new_acc(CcId, Ontology)], cc(Me, CcId, Ontology)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(mqtt_send(CcId, To, Onto, Predicate), [], sent(CcId, message(To, Ontology, Predicate))).





