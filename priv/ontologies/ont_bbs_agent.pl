%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Initialisation

%% @doc: initialized(+Ns, +Ag, +Params)
%% Initialize this ontology

action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).

initialize(AgentId, Namespace, Params) :-
    log(info,"Waking up agent :~p",[AgentId]),
     new_knowledge_base("bbs:agent:event_handlers"),
     new_knowledge_base("bbs:agent:stims"),
     new_knowledge_base("bbs:agent:ccs", erlog_db_dict),
     assert(me(AgentId)),

    assert(initialized(Ns, Ag, Params)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen fsm events related predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc: agent_event_processed(+Type, +Message)
%% execute all stored reactions liked to erlang fsm event ( ex cast, call, info ) with pattern Message
%% +Type match erlang fsm event type like this :
%% call -> sync_event
%% cast -> async_event
%% info -> info_event

action(process_agent_event(Type, Message), [log(info,"Processing event : ~p",[{Type, Message}])], agent_event_processed(Type, Message)).

%% @doc: process_agent_events(+Type, +Message)
%% Effectively execute stims and delete needed ones

process_agent_event(Type, Message) :-
    \+do_process_agent_event(Type, Message).

do_process_agent_event(Type, Message) :-
    Head = event(Type, Message, Options),
    "bbs:agent:event_handlers"::clause(Head, Body),
    cutted_goal("bbs:agent:event_handlers"::Head),
    member(once, Options),
    "bbs:agent:event_handlers"::retract(( Head :- Body )),
    log(info,"FINISHED",[]),
    fail.

cutted_goal(Goal) :-
    Goal,!.

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

action(process_stim(StimOnt, StimMessage), [log(info,"Processing stim :~p",[StimMessage])], stim_processed(StimOnt, StimMessage)).

%% @doc: process_stim(+StimOnt, +StimMessage)
%% Effectively execute stim reactions and those that needs to be.

process_stim(StimOnt, StimMessage) :-
                              \+do_process_stim(StimOnt, StimMessage).



do_process_stim(StimOnt, StimMessage) :-
    Head = stim(StimOnt, StimMessage, Options),
              log(info, "Head : ~p", [Head]),

    "bbs:agent:stims"::clause(Head, Body),

    cutted_goal("bbs:agent:stims"::Head),
                      log(info, "checing option ", []),

    member(once, Options),
                          log(info, "retracting option ", []),

    "bbs:agent:stims"::retract(( Head :- Body )),
    fail.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Messaging related predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


action(process_incoming_message(Com_Channel, From, To, Ontology, Predicate),[],
    incoming_message_processed(Com_Channel, From, To, Ontology, Predicate)).

process_incoming_message(Cc, From, To, Ontology, Predicate) :-
    goal(stim_processed("bbs:agent", message(Cc, From, To, Ontology, Predicate))).

action(Transport_Ontology::goal(sent_message(CcId, message(To, Ontology, Predicate))),
         [message_transport_ontology(Transport_Ontology), Transport_Ontology::cc(To, CcId, Ontology)],
            sent(CcIc, message(To, Ontology, Predicate))).


