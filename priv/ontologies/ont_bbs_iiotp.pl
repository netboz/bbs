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



%%%%%%%%%%%%%%% goal stuff

goal(Goal) :-
    call(Goal).

goal(Goal) :-
     log(info, "Looking to satisfy Goal : ~p", [Goal]),
     action(Action, Prereq, Goal),
     %%log(debug, "Next action :~p",[Action]),
     %%log(debug, "prereqs ~p",[Prereq]),
     satisfy_prereq(Prereq),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).


%%%%%%%%%%%%%%%%
goal(Onto, Goal) :-
    prove_external_ont(Onto, Goal).

goal(Onto, Goal) :-
    log(info, "Looking to satisfy Goal : ~p ~p", [Onto, Goal]),
    prove_external_ont(Onto, action(Action, Prereq, Goal)),
    log(debug, "prereqs ~p",[Prereq]),
    satisfy_prereq(Onto, Prereq),
    log(debug,"Prereqs ok performing action :~p",[{Onto,Action}]),
    prove_external_ont(Onto, Action).

satisfy_prereq(Onto, [Goal1|Others]) :-
    goal(Onto, Goal1),
    satisfy_prereq(Onto, Others).
satisfy_prereq(Onto, []).
