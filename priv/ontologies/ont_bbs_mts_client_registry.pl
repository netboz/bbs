%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(assert(initialized(Ns, Ag, Params)),
    [
    log(info,"Starting Reg ~p",[{Ag, Node, Ns, Params}]),
    %% Add hook to be notified of registry messages
    "bbs:agent"::react_on(info_event, registry_delivery(Payload),
        "bbs:mts:client:registry", process_incoming_registry_payload(Payload)),    

    %% Add this registry to bbs:agent centralized registry
    "bbs:agent:ccs"::assert(( cc(CcId, LookedAgent, LookedNode, "localhost", "bbs:mts:client:registry") :-
     "bbs:mts:client:registry"::cc(CcId, LookedAgent, LookedNode, "localhost", "bbs:mts:client:registry") )),

    %% Register this ontology as being able to send message
    "bbs:agent"::assert(message_transport_ontology("bbs:mts:client:registry")),

    subscribe("test")
    %"bbs:agent"::goal(sent("test", "Hello World"))
    ],
    initialized(Ag, Node, Ns, Params)).



process_incoming_registry_payload(Payload) :-
    log(info,"Received payload :~p", [Payload]).




subscribed(CcId) :-
    me(Me),
    node(Node),
    cc(CcId, Me, Node).

subscribed(CcId, Domain) :-
    connection(Domain, _, _, Pid, _),
    subscribed(CcId, Pid).

    