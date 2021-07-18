%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(AgentId, Parent, NameSpace, Params),
    [
    log(info,"Initialising mqtt client",[]),
    assert(me(AgentId)),
    assert(parent(Parent)),
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:mqtt")),


    log(info,"Connecting",[]),
    connect([], AgentId, Pid),
        log(info,"ClientId   ~p",[ClientId]),

    mqtt_subscribe(Pid, "test_topic")
    ],
    initialized(AgentId, Parent, NameSpace, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    assert(initialized(AgentId, Parent, NameSpace, Params)).


%% ClientId is unique per Ontology, this may not reflect reality where two clients connecting on two servers using same naming
%% Conventions can end up with two clients with different hosts but same clientId
action(assert(client(Options, Pid)), [new_client(Options, Pid), options_processed(Pid, Options)], client(Options, Pid)).

options_processed(Pid, [host(Host) | OtherOptions]) :-
    assert(host(Pid, Host)),
    options_processed(OtherOptions).

options_processed(Pid, [port(Port) | OtherOptions]) :-
    assert(port(Pid, Port)),
    options_processed(OtherOptions).

options_processed(Pid, [clientid(ClientId) | OtherOptions]) :-
    assert(clientid(Pid, ClientId)),
    options_processed(OtherOptions).

action(assert(connected(Pid)),
    [connect(Pid)],
    connected(pid(Pid))).

action(assert(connected(Pid)),
    [clientid(Pid, ClientId), connect(Pid)],
    connected(clientid(ClientId))).


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



