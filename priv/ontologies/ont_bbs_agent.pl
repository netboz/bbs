%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc: initialized(+Ns, +Ag, +Params)
%% Initialize this ontology

action(initialize(Ns, Ag, Params), [
    new_knowledge_base("bbs:agent:event_handlers")
    ], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    log(info,"Waking up agent :~p",[AgentId]),
    assert(initialized(Ns, Ag, Params)).

action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    assert(booted(Ns, Ag, Params)).


%% @doc: agent_event_processed(+Type, +Message)
%% execute all stored reactions liked to erlang fsm event ( ex cast, call, info ) with pattern Message

action(process_agent_event(Type, Message), [], agent_event_processed(Type, Message)).

%% @doc: process_agent_events(+Type, +Message)
%% Effectively execute stims and delete needed ones

process_agent_events(Type, Message) :-
    \+process_events(Type, Message).

process_events(Type, Message) :-
    Head = event(type, Message, Options),
    "bbs:agent:events"::clause(Head, Body),
    "bbs:agent:events"::Head,
    member(once, Options),
    "bbs:agent:events"::retract(( Head :- Body )),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen fsm events related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc: Set a callback to gen_fsm events.
%% Set a hook to gen_fsm:handle(Type, Event_patten, _State) that prove Ontology_reaction:Predicate
%% Type can be one of sync_event, async_event, info_event, to avoid collision with erlog 'call' predicate

react_on(Event_type, Event_patten, Ontology_reaction, Predicate) :-
    react_on(Event_patten, Ontology_reaction, Predicate, []).

react_on(Event_type, Event_patten, Ontology_reaction, Predicate, Options) :-
    "bbs:agent:event_handlers"::assert(( event(Event_type, Event_patten, Options) :-
    Ontology_reaction::Predicate )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Message sending predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


action(Transport_Ontology::send_message(Dest, Acc, Onto, Message),
         [message_transport_ontology(Transport_Ontology)],
            sent(Dest, Acc, Onto, Message)).



