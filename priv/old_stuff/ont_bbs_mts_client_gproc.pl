%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [
    %% Add hook to receive MQTT messages from mqtt client library
    "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, Payload, Ts)),
        %% At this moment we are not managing QOS and headers, this will come
        "bbs:mts:mqtt:client", process_incoming_mqtt_message(Id, Ts, From, Topic, Payload)),

    goal(cc("test", "bbs:agent")),
    "bbs:agent"::react_on_stim("bbs:agent",
        message(Cc, From, To, Ontology, Predicate), "bbs:mts:mqtt:client", test_stim(Ontology, Predicate)) ,


    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:mqtt:client"))
    ],
    initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
     assert(me(AgentId)),
    assert(initialized(Ns, Ag, Params)).

test_stim(Ontology, Predicate) :-
    log(info,"TESTINGT",[]),
    call(Predicate),
    log(info,"TESTINGT ",[]).


%% As agent subscribed to the topic, it receive a copy of the messages it posts. This is to igonre them...for now
process_incoming_mqtt_message(Id, Ts, From, Topic, Payload) :-
    "bbs:agent"::me(From).
%% Manage payloads in prolog term formats. TODO: Add correct encoding management.
process_incoming_mqtt_message(Id, Ts, From, Topic, Payload) :-
    type_of(Payload, predicate),
    log(info,"Incoming mqtt message :~p",[Payload]),
    topic_ccid(Topic, CcId),
    "bbs:agent:ccs"::cc("bbs:mts:mqtt:client", CcId, To, Ontology),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, Payload)).

%% Manage payloads in string format
process_incoming_mqtt_message(Id, Ts, From, Topic, Payload) :-
    type_of(Payload, string),
    log(info,"Incoming mqtt prolog message ~p   ~p",[Topic, Payload]),
    topic_ccid(Topic, CcId),
    "bbs:agent:ccs"::cc("bbs:mts:mqtt:client", CcId, To, Ontology),
    "bbs:agent"::prolog_interpretation(Payload, MessagePrologTerms),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, MessagePrologTerms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action("bbs:agent:ccs"::assert(cc("bbs:mts:mqtt:client", CcId, Me, Ontology)),
    ["bbs:agent"::me(Me), \+"bbs:agent:ccs"::cc(_, CcId, _, _), new_cc(CcId, Ontology)],
        cc(CcId, Ontology)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(mqtt_send(CcId, Message), [log(info,"SEning",[])], sent(CcId, Message)).





