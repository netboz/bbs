%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Initialisation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(initialize(Ns, Ag, Params), [], initialized(Ns, Ag, Params)).
initialize(Ns, Ag, Params) :-
    log(info,"Bubble ontology initialized",[]),
    assert(initialized(Ns, Ag, Params)).



action(boot(Ns, Ag, Params), [], booted(Ns, Ag, Params)).

boot(Ns, Ag, Params) :-
    log(info,"Bubble entering itself",[]),
    goal(inside_bubble(Ag)),
    assert(booted(Ns, Ag, Params)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%% World Entering %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(enter_bubble(AgentName, Body, Model),[
                            have_model(AgentName, Model),
                            have_body(AgentName,Body)
                            %% TODO remove hardcoded bubble
                            ],
                             inside_bubble(AgentName)).

enter_bubble(AgentName,Body, Model) :-
    %%register_shape(AgentName, Body),
    log(info,"~p reauest entering the bubble",[AgentName]),
    log(info,"Body is :~p",[Body]),
    !,

    "bbs:bbs_agent"::goal("bbs:physics",registered_body(AgentName, Body)),
    "bbs:physics"::add_body_to_world(AgentName,"default_world_name",1,1),
    "bbs:physics"::create_cone_viewpoint(AgentName,"default_vp"),
    log(info,"~p Entered the bubble",[AgentName]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% have_model %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(request_model(AgentName, Model) ,[] ,have_model(AgentName, Model)).

request_model(AgentName, Model) :-
    me(From),
    "bbs:bbs_agent"::goal(
                          query_processed(From, AgentName, "bbs:physics:entity", model(AgentName, Model), Model)
                           ),
     assert(have_model(AgentName,Model)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% have body %%%%%%%%%%%%%%%%%%%%%%%%%%%

action(request_body(AgentName, Body),[],have_body(AgentName, Body)).

request_body(AgentName, Body) :-
    me(From),
    "bbs:bbs_agent"::goal(
                          query_processed(From, AgentName, "bbs:physics:entity", body(AgentName, Body), Body)
                         ),
     assert(have_body(AgentName,Body)).


register_shape(shape(ShapeName,Shape)) :-
    log(info,"****************** REGISTERING SHAPE :~p",[ShapeName]),
    goal("bbs:physics",registered_shape_body(ShapeName,Shape)).



initialize_environment(Connection) :-
    environment_as_list(EnvList),
    send_environment(Connection, EnvList).


gravity(0.9).
bubble_model("bulle_asset.kiko").


environment_as_list(EnvList) :-
    bubble_model(Model),
    EnvList = [[model, Model]].


%%%%%%%%%%%%%%%%%%%%%%%%%

goal(Goal) :-
    call(Goal).

goal(Goal) :-
     %%log(debug, "Looking to satisfy Goal : ~p", [Goal]),
     action(Action, Prereq, Goal),
             log(info, "action ~p",[Action]),
     %%log(debug, "prereqs ~p",[Prereq]),
     satisfy_prereq(Prereq),
     log(debug,"Prereqs ok performing action :~p",[{Onto,Action}]),
     call(Action).

satisfy_prereq([]).

satisfy_prereq([Goal1|Others]) :-
     goal(Goal1),
     satisfy_prereq(Others).

goal(Onto, Goal) :-
	log(info, "Cheking if :~p  ~p", [Onto,Goal]),
    prove_external_ont(Onto, Goal).

goal(Onto, Goal) :-
    log(debug, "Looking to satisfy Goal : ~p ~p", [Onto, Goal]),
    prove_external_ont(Onto, action(Action, Prereq, Goal)),
    log(debug, "prereqs ~p",[Prereq]),
    satisfy_prereq(Onto, Prereq),
    log(debug,"Prereqs ok, performing action :~p",[{Onto,Action}]),
    prove_external_ont(Onto, Action).

satisfy_prereq(Onto, [Goal1|Others]) :-
    goal(Onto, Goal1),
    satisfy_prereq(Onto, Others).
satisfy_prereq(Onto, []).




