%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(initialize(Ns, Ag, Params),
    [start_mqtt_internal(StartResult), log(info,"Starting bubmqtt broker :~p",  [StartResult])],
        initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    assert(initialized(Ns, Ag, Params)).

