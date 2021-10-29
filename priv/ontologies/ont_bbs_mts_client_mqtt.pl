%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, Node, Params),
    [
    log(info,"Initialising mqtt client",[]),
    assert(me(AgentId)),
    assert(parent(Parent)),
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:mqtt")),
    goal(connection(Domain, Port, ClientId, Pid, [])),
    log(info, "Domain ~p    Port ~p    ClientId ~p    Pid ~p", [Domain, Port, ClientId, Pid])
    ],
    initialized(AgentId, Parent, Node, Params)).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% new mqtt cc %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(join_cc(CcId, Ontology, Pid),
    [\+"bbs:agent:ccs"::cc(_, CcId, _, _, _), host(Pid, Host)],
        cc(CcId, Ontology, Host)).

action(join_cc(CcId, Ontology),
    [\+"bbs:agent:ccs"::cc(_, CcId, _, _, _), client(Pid, "localhost")],
        cc(CcId, Ontology)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% send a message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(mqtt_send(CcId, Message), [], sent(CcId, Message)).

cc(CcId, Ontology) :-
    me(Me),
    parent(Parent),
    cc(CcId, Me, Ontology, Parent).



