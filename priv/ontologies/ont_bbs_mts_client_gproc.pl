%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params),
    [
    assert(me(AgentId)),
    assert(parent(Parent)),
    %% Add hook to receive MQTT messages from mqtt client library
%    "bbs:agent"::react_on(info_event, deliver(Topic, message(Id, _QOS, From, _Flags, _Headers, Topic, Payload, Ts)),
%        %% At this moment we are not managing QOS and headers, this will come
%        "bbs:mts:mqtt:client", process_incoming_mqtt_message(Id, Ts, From, Topic, Payload)),

    %goal(cc("test", "bbs:agent")),
    %"bbs:agent"::react_on_stim("bbs:agent",
    %    message(Cc, From, To, Ontology, Predicate), "bbs:mts:mqtt:client", test_stim(Ontology, Predicate)) ,
    %register_gproc(Parent),
    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:gproc"))
          %  goal(cc(AgentId, "bbs:agent"))
    ],
    initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
     "bbs:agent:ccs"::assert(( cc("bbs:mts:mqtt:client", CcId, To, Ontology, Parent) :-
                                    "bbs:mts:mqtt:client"::cc(CCId, To, Ontology, Parent))),
    assert(initialized(AgentId, Parent, NameSpace, Params)).



goal(join_cc_predicate(CcId, Ontology),[\+"bbs:agent:ccs"::cc(CcId, _, _, _)], cc(CcId, Ontology)).






















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
action(join_cc(CcId, Ontology),
    [\+"bbs:agent:ccs"::cc(_, CcId, _, _, _)],
        cc(CcId, Ontology)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(gproc_send(CcId, Message), [], sent(CcId, Message)).

cc(CcId, Ontology) :-
    me(Me),
    parent(Parent),
    cc(CcId, Me, Ontology, Parent).



