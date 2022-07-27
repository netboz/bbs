%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Initialisation

%% @doc: initialized(+Ns, +Ag, +Params)
%% Initialize this ontology

action(initialize(AgentId, Parent, Node, Params), [], 
            initialized(AgentId, Parent, Node, Params)).

initialize(AgentId, Parent, NameSpace, Params) :-
    log(info,"Waking up agent :~p",[AgentId]),
     new_knowledge_base("bbs:agent:event_handlers"),
     new_knowledge_base("bbs:agent:stims"),
     new_knowledge_base("bbs:agent:ccs", erlog_db_dict),
     assert(me(AgentId)),
     assert(parent(Parent)),
     assert(node(Node)),
     assert(initialized(AgentId, Parent, NameSpace, Params)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen fsm events related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc: agent_event_processed(+Type, +Message)
%% execute all stored reactions liked to erlang fsm event ( ex cast, call, info ) with pattern Message
%% +Type match erlang fsm event type like this :
%% call -> sync_event
%% cast -> async_event
%% info -> info_event

action(process_agent_event(Type, Message), 
    [log(debug,"Processing event : ~p",[{Type, Message}])], 
    agent_event_processed(Type, Message)).

%% @doc: process_agent_events(+Type, +Message)
%% Effectively execute stims and delete needed ones

process_agent_event(Type, Message) :-
    \+do_process_agent_event(Type, Message).

do_process_agent_event(Type, Message) :-
    Head = event(Type, Message, Options),
    "bbs:agent:event_handlers"::clause(Head, Body),
    "bbs:agent:event_handlers"::Head,
    !,
    member(once, Options),
    "bbs:agent:event_handlers"::retract(( Head :- Body )),
    fail.

%% @doc: react_on(+Event_type, +Event_patten, +Ontology_reaction, +Predicate)
%% Set a hook to gen_fsm:handle(Type, Event_patten, _State) that prove Ontology_reaction:Predicate
%% Type can be one of sync_event, async_event, info_event, to avoid collision with erlog 'call' predicate

react_on(Event_type, Event_patten, Ontology_reaction, Predicate) :-
    react_on(Event_type, Event_patten, Ontology_reaction, Predicate, []).

react_on(Event_type, Event_patten, Ontology_reaction, Predicate, Options) :-
    "bbs:agent:event_handlers"::assert(( event(Event_type, Event_patten, Options) :-
    Ontology_reaction::Predicate )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Stims related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc: react_on_stim(+Event_type, +Event_patten, +Ontology_reaction, +Predicate)
%% Set an Ontology::predicate to be called when stim StimOnt::StimPred is triggered

react_on_stim(StimOnt, StimPredicate, ReactionOnt, ReactionPredicate) :-
    react_on_stim(StimOnt, StimPredicate, ReactionOnt, ReactionPredicate, []).

react_on_stim(StimOnt, StimPredicate, ReactionOnt, ReactionPredicate, Options) :-
    "bbs:agent:stims"::assert(( stim(StimOnt, StimPredicate, Options) :-
    ReactionOnt::ReactionPredicate )).

%% @doc: stim_processed(+StimOnt, +StimMessage)
%% execute all stored reactions liked to StimOnt::StimMessage

action(process_stim(StimOnt, StimMessage),
    [log(info,"Processing stim : ~p ~p",[StimOnt, StimMessage])],
        stim_processed(StimOnt, StimMessage)).

%% @doc: process_stim(+StimOnt, +StimMessage)
%% Effectively execute stim reactions and those that needs to be.

process_stim(StimOnt, StimMessage) :-
    \+do_process_stim(StimOnt, StimMessage).

do_process_stim(StimOnt, StimMessage) :-
    Head = stim(StimOnt, StimMessage, Options),
    "bbs:agent:stims"::clause(Head, Body),
    cutted_goal("bbs:agent:stims"::Head),
    member(once, Options),
    "bbs:agent:stims"::retract(( Head :- Body )),
    fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% other agents related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Messaging related predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc: cc(CcId:IN:string())
%% This action the create Communication Channels (CCs) locally using default "bbs:mts:client:registry" ontology

action("bbs:mts:client:registry"::subscribe(CcId),[], subscribed(CcId)).
action(TransportOntology::goal(subscribe(CcId)), [], subscribed(CcId, TransportOntology)).
action(TransportOntology::goal(subscribe(CcId, Domain)), [], subscribed(CcId, Domain, TransportOntology)).

subscribed(CcId) :-
    subscribed(CcId, "bbs:mts:client:registry").
subscribed(CcId, TransportOntology) :-
    TransportOntology::subscribed(CcId).
subscribed(CcId, Domain, TransportOntology) :-
    TransportOntology::subscribed(CcId, Domain).


%cc(CcId, Agent, Node, Domain, ClientId, TransportOntology)
%% @doc: sent(CcId::string(), Payload:: term())
%% This action sends Payload content on communication channel CcId

action(TransportOntology::send(CcId, Payload),[
    log(info, "Before CC",[]),
    "bbs:agent:ccs"::cc(CcId, _, _, TransportOntology),     
    log(info, "after CC",[])], 
    sent(CcId, Payload)).

action(process_incoming_data(CcId, Data),
    [],
    incoming_data_processed(CcId, Data)).

process_incoming_data(CcId, Data) :-
    goal(stim_processed("bbs:agent", incoming_data(CcId, Data))).

%% @doc: message_sent(CcId::string(), message(To::term(), Ontology:string(), Payload:term()))
%% This action sends 'message(To, Ontology, Payload)' on specified CcId.
%% Transport ontology will verify 'To' can be reached on this CcId, this means this predicate will fail if 
%% "TransportOntology"::to(To, CcId) fails 

action(TransportOntology::send(CcId, message(agent(Node, AgentId), Ontology, Payload)),
    ["bbs:agent:ccs"::cc(CcId, AgentId, Node, TransportOntology)], 
        message_sent(CcId, message(agent(Node, AgentId), Ontology, Payload))).



action(process_incoming_message(Com_Channel, From, To, Ontology, Predicate),
    [],
    incoming_message_processed(Com_Channel, From, To, Ontology, Predicate)).

process_incoming_message(Cc, From, To, Ontology, Predicate) :-
    goal(stim_processed("bbs:agent", message(Cc, From, To, Ontology, Predicate))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% bbs:agent Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



test(test_react_on_stim, AgentId) :-
    log(info, "Running test: ~p", [test_react_on_stim]),
    react_on_stim("bbs:agent", stim_test_react_on(Value), "bbs:agent", react_on_stim_test(Value)),
    goal(stim_processed("bbs:agent", stim_test_react_on(test_value1))),
    test_react_on_check(test_value1),
    %% Check stim is still present after being first triggered
    goal(stim_processed("bbs:agent", stim_test_react_on(test_value2))),
    %% Do some cleaning, remove the stim
    Head = stim("bbs:agent", stim_test_react_on(Value), []),
    Body = "bbs:agent"::react_on_stim_test(Value),
    "bbs:agent:stims"::retract(( Head :- Body )),
    retract(test_react_on_check(test_value1)),
    retract(test_react_on_check(test_value2)),

    %% Validate we have received second steam
    test_react_on_check(test_value2),
    log(info, "Test succeeded: ~p", [test_react_on_stim]).


 test(test_react_on_stim_once, AgentId) :-
     log(info, "Running test: ~p", [test_react_on_stim_once]),
     %% Set Stim, but notice 'once' parameter option for the stim to be single user
     react_on_stim("bbs:agent", stim_test_react_on(Value), "bbs:agent", react_on_stim_test(Value), [once]),
     %% Trigger the stim
     goal(stim_processed("bbs:agent", stim_test_react_on(test_value1))),
     test_react_on_check(test_value1),
     %% Now checks stim isn't active anymore
     goal(stim_processed("bbs:agent", stim_test_react_on(test_value2))),
     \+test_react_on_check(test_value2),
     log(info, "Test succeeded: ~p", [test_react_on_stim_once]).


 react_on_stim_test(Value) :-
    assert(test_react_on_check(Value)).
