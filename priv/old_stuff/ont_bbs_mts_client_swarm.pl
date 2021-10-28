%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [
    ],
    initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(me(AgentId)),
    assert(initialized(Ns, Ag, Params)).


%% As agent subscribed to the topic, it receive a copy of the messages it posts. This is to igonre them...for now
process_incoming_swarm_message(Id, Ts, From, Topic, Payload) :-
    "bbs:agent"::me(From).
%% Manage payloads in prolog term formats. TODO: Add correct encoding management.
process_incoming_swarm_message(Id, Ts, From, Topic, Payload) :-
    type_of(Payload, predicate),
    log(info,"Incoming mqtt message :~p",[Payload]),
    topic_ccid(Topic, CcId),
    "bbs:agent:ccs"::cc("bbs:mts:mqtt:client", CcId, To, Ontology),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, Payload)).

%% Manage payloads in string format
process_incoming_swarm_message(Id, Ts, From, Topic, Payload) :-
    type_of(Payload, string),
    log(info,"Incoming mqtt prolog message ~p   ~p",[Topic, Payload]),
    topic_ccid(Topic, CcId),
    "bbs:agent:ccs"::cc("bbs:mts:mqtt:client", CcId, To, Ontology),
    "bbs:agent"::prolog_interpretation(Payload, MessagePrologTerms),
    "bbs:agent"::goal(incoming_message_processed(Topic, From, To, Ontology, MessagePrologTerms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action("bbs:agent:ccs"::assert(cc("bbs:mts:client:swarm", CcId, Me, Ontology)),
    [me(Me), \+"bbs:agent:ccs"::cc(_, CcId, _, _), new_cc(CcId, Ontology)],
        cc(CcId, Ontology)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(swarm_send(CcId, Message), [], sent(CcId, Message)).





