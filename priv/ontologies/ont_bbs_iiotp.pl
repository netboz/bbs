namespace("bbs:iiotp").

%%%%%%%%%%%%%%% Ontology Initialisation

%% Ontology initialization
action(initialize(Ag, NsPred,Pred), [handler(NsPred, Pred, Params)], initialized(Ns, Ag, Params)).


handler(NsPred, Pred, Params) :-
    key_value(Params, handler_ont, NsPred, true),
    key_value(Params, handler_pred, Pred, true).


handler(NsPred, Pred, Params) :-
    default_handler(NsPred, Pred).

default_handler("bbs:iiotp",default_public_message_handler(OriginAid, DestinationAid, Ont, Message)).


initialize(Ag, NsPred, Pred) :-
    "bbs:bbs_agent"::my_aid_as_map(M),
    new_public_acc(Acc),
    react_on("bbs:iiotp",iiotp_message(Acc, Origin, Destination, Ont, Message),
        "bbs:iiotp", default_public_message_handler(Origin, Destination, Ont, Message),[]),
    assert(Acc),
    assert(initialized(Ns, Ag, Params)).

%% TODO : using var Ont because ontology is received as a binary .. don't know yet how to fix

%%default_public_message_handler(OriginAid, DestinationAid, Ont, inform(_Sender, _MyAid, Ont, Predicate, Params)) :-
%%    "bbs:bbs_agent"::trigger_stims(Ont, inform(Predicate)).

%%default_public_message_handler(OriginAid, DestinationAid, Ont, Message) :-
%%    log(info,"Default behavior for message :~p:~p",[Ont,Message]),
%%    prove_external_ont(Ont,Message).

default_public_message_handler(OriginAid, DestinationAid, Ont, Message) :-
    log(info,"default handler trigguering stims : ~p",[Message]),
    "bbs:bbs_agent"::trigger_stims(Ont, Message).


action(boot(Name),[],booted(Ns, Name,Opts)).


boot(Ag).

%%%%%%%%%%%%%%% sending messages

action(send_msg_iiotp(FromAid, DestAid, ToAcc, Onto, Message),
        [
        "bbs:bbs_agent"::public_aid_as_map(FromName, FromAid),
        "bbs:bbs_agent"::public_aid_as_map(ToName, DestAid),
        public_acc(ToName, ToAcc)],
            sent(FromName, ToName, Onto, Message)).

