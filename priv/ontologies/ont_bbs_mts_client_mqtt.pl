%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, Node, Params),
    [
    log(info,"Initialising mqtt client",[]),
    assert(me(AgentId)),
    assert(parent(Parent)),
    assert(node(Node)),
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:mqtt")),
    pairs_key_value(Params, clients(ClientList)),
    connections_initiated(ClientList)
    ],
    initialized(AgentId, Parent, Node, Params)).


connections_initiated([]).
connections_initiated([connection(Domain, Port, ClientId, Options, Subscriptions)|OtherConnections]) :-
    goal(connection(Domain, Port, ClientId, Pid, Options)),
    subscriptions_initiated(Pid, Subscriptions),
    connections_initiated(OtherConnections).

subscriptions_initiated(ClientPid, []).
subscriptions_initiated(ClientPid, [subscription(Topic)|OtherSubscriptions]) :-
    subscribe(ClientPid, Topic),
    subscriptions_initiated(Domain, OtherSubscriptions). 


initialize(AgentId, Parent, Node, Params) :-
    assert(initialized(AgentId, Parent, Node, Params)).


%%connection(domain, port, ClientId, Pid).
%cc(CcId, Agent, Node, Domain, ClientId, TransportOntology)

action(assert(connection(Domain, Port, ClientId, Pid, Connection_options)), 
        [
            log(info,"Prolog connect", []),
            connect(Domain, Port, ClientId, Pid, Connection_options)
        ], 
    connection(Domain, Port, ClientId, Pid, Connection_options)).



subscribed(Topic) :-
    subscribed(Topic, "localhost").

subscribed(Topic, Domain) :-
    connection(Domain, _, _, Pid, _),
    mqtt_subscribed(Topic, Pid).




action(mqtt_publish(Pid, Topic, Payload, Options),[connection(Domain, _, ClientId, Pid, _)], 
    published(Payload, Topic, Domain, Options)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(assert(cc(CcId, Me, Node, Domain, ClientId)),
    [
    me(Me),
    node(Node),
    connection(Domain, _Port, ClientId, Pid, _Connection_options),
    subscribed(CcId, Pid)
    ],
    cc(CcId, ClientId)).

%% action(join_cc(CcId, Ontology),
%%     [\+"bbs:agent:ccs"::cc(_, CcId, _, _, _), client(Pid, "localhost")],
%%         cc(CcId, Ontology)).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(mqtt_send(CcId, Message), [], sent(CcId, Message)).




%cc(CcId, Ag, Node, Domain, ClientId).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pairs_key_value([Predicate|_], Pattern) :-
    Pattern = Predicate.
pairs_key_value([Predicate|OtherPredicates], Pattern) :-
    pairs_key_value(OtherPredicates, Pattern).
pairs_key_value(_, _).

