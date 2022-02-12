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
    ],
    initialized(Ag, Node, Ns, Params)).



process_incoming_registry_payload(Payload) :-
    log(info,"Received payload :~p", [Payload]).



subscribed(CcId) :-
    me(Me),
    node(Node),
    cc(CcId, Me, Node).


action(update_aid(Key, Value, OldValue),[log(info," checking for update",[]), key(Key)], aid(Key, Value)).
action(set_aid(Key, Value),[log(info, "Setting aid",[])], aid(Key, Value)).

update_aid(Key, Value, OldValue) :-
    log(info," Updating",[]),
    update_registry_entry(Key, Value),
    "bbs:agent"::goal(stim_processed("bbs:mts:client:registry", aid_updated(Key, OldValue, Value))).

set_aid(Key, Value) :-
    log(info," New Aid",[]),
    new_registry_entry(Key, Value),
    "bbs:agent"::goal(stim_processed("bbs:mts:client:registry", new_aid(Key, Value))).

