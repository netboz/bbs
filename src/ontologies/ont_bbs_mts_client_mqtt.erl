%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2021 10:16 PM
%%%-------------------------------------------------------------------
-module(ont_bbs_mts_client_mqtt).

-author("yan").

-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

-define(DEFAULT_TIMEOUT, 60 * 1000).

%% Prolog API

-define(ERLANG_PREDS,
        [{{connect, 5}, ?MODULE, connect_predicate},
         {{connect_async, 2}, ?MODULE, connect_async_predicate},
         {{mqtt_publish, 4}, ?MODULE, mqtt_publish_predicate},
         {{mqtt_subscribed, 2}, ?MODULE, mqtt_subscribed_predicate},
         {{subscribe, 2}, ?MODULE, mqtt_subscribe_predicate},
         {{new_cc, 2}, ?MODULE, new_cc_predicate},
         {{mqtt_send, 2}, ?MODULE, mqtt_send_predicate},
         {{topic_ccid, 2}, ?MODULE, topic_ccid_predicate}]).

%% API
-export([external_predicates/0]).
-export([perform_connect/3, msg_handler/2, disconnected/1]).
-export([connect_predicate/3, connect_async_predicate/3]).
-export([mqtt_subscribed_predicate/3, mqtt_subscribe_predicate/3,
         mqtt_publish_predicate/3]).
-export([new_cc_predicate/3, mqtt_send_predicate/3, topic_ccid_predicate/3]).

external_predicates() ->
    ?ERLANG_PREDS.

%new_client_predicate({_, Host, Port, PidClientOut}, Next0, #est{bs = Bs} = St) ->

connect_predicate({_, Domain, Port, ClientId, {_} = Pid, Connection_options},
                  Next0,
                  #est{bs = Bs0} = St) ->
    ?DEBUG("Conneting to mqtt", []),
    [DClientId, DConnection_options] = erlog_int:dderef([ClientId, Connection_options], Bs0),

    case erlog:vars_in(DConnection_options) of
        [] ->
            {ClientDomain, Bs1} =
                case erlog_int:dderef(Domain, Bs0) of
                    {_} ->
                        DBs = erlog_int:add_binding(Domain, "localhost", Bs0),
                        {"localhost", DBs};
                    Domain when is_binary(Domain) ->
                        {binary_to_list(Domain), Bs0};
                    _ ->
                        erlog_int:fail(St)
                end,
            {ClientPort, Bs2} =
                case erlog_int:dderef(Port, Bs1) of
                    {_} ->
                        PBs = erlog_int:add_binding(Port, 1883, Bs1),
                        {1883, PBs};
                    Port when is_integer(Port) ->
                        {Port, Bs1};
                    _ ->
                        erlog_int:fail(St)
                end,

            AgentPid = self(),
            ClientIdEntry =
                case DClientId of
                    {_} ->
                        [];
                    _ ->
                        [{clientid, DClientId}]
                end,

            Options2 =
                lists:keymerge(1,
                               [{msg_handler,
                                 #{disconnected => fun disconnected/1,
                                   publish => {?MODULE, msg_handler, [AgentPid]}}},
                                {owner, AgentPid},
                                {host, ClientDomain},
                                {port, ClientPort}]
                               ++ ClientIdEntry,
                               DConnection_options),

            case gen_statem:start(emqtt, [Options2], []) of
                {ok, PidClient} ->
                    case emqtt:connect(PidClient) of
                        {ok, _Props} ->
                            FinalClientID =
                                proplists:get_value(clientid, gen_statem:call(PidClient, info)),
                            case erlog_int:unify(FinalClientID, ClientId, Bs2) of
                                {succeed, NewBs} ->
                                    %% This should always be true
                                    ?INFO_MSG("Client connected ~p", [FinalClientID]),
                                    erlog_int:unify_prove_body(Pid,
                                                               PidClient,
                                                               Next0,
                                                               St#est{bs = NewBs});
                                _ ->
                                    emqtt:disconnect(PidClient),
                                    erlog_int:fail(St)
                            end;
                        {error, Reason} ->
                            ?INFO_MSG("Error connecting mqtt client ~p", [Reason]),
                            erlog_int:fail(St)
                    end;
                {error, _Error} ->
                    ?INFO_MSG("Error strating mqtt client ~p", [_Error]),
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end;
connect_predicate({_, _Domain, _Port, _ClientId, _, _Connection_options}, _Next0, St) ->
    erlog_int:fail(St).

msg_handler(Msg, Pid) ->
    ?INFO_MSG("Processing publish ~p to ~p", [Msg, Pid]),
    Pid
    ! {incoming_mqtt_message,
       maps:get(qos, Msg),
       maps:get(dup, Msg),
       maps:get(retain, Msg),
       maps:get(packet_id, Msg),
       maps:get(topic, Msg),
       maps:get(payload, Msg)}.

disconnected(Data) ->
    ?INFO_MSG("DISCONNECTED ~p", [Data]).

mqtt_subscribed_predicate({_, Topic, ClientPid}, Next0, #est{bs = Bs} = St) ->
    [DTopic, DClientPid] = erlog_int:dderef([Topic, ClientPid], Bs),
    ?DEBUG("mqtt_subscribed_predicate    ~p    ~p", [DTopic, DClientPid]),

    case erlog:vars_in(DClientPid) of
        [_] ->
            %% ClientPid must be binded
            erlog_int:fail(St);
        _ ->
            case erlog:vars_in(DTopic) of
                [_] ->
                    RawResult = emqtt:subscriptions(DClientPid),
                    % Rawresult : [{topic(), [subopt()]}]
                    mqtt_subscribed_predicate_browse(DTopic, DClientPid, Next0, St, RawResult);
                [] ->
                    RawResult = emqtt:subscriptions(DClientPid),
                    ?INFO_MSG("Subscriptions ~p", [RawResult]),
                    case lists:keyfind(DTopic, 1, RawResult) of
                        false ->
                            ?INFO_MSG("Failling Subscriptions ~p", [RawResult]),
                            erlog_int:fail(St);
                        _ ->
                            erlog_int:prove_body(Next0, St)
                    end
            end
    end.

mqtt_subscribed_predicate_browse(_DTopic, _DClientPid, _Next0, St, []) ->
    erlog_int:fail(St);
mqtt_subscribed_predicate_browse(DTopic,
                                 DClientPid,
                                 Next0,
                                 #est{bs = Bs,
                                      vn = Vn,
                                      cps = Cps} =
                                     St,
                                 [{RTopic, _Params} | OtherTopics]) ->
    case erlog_int:unify(DTopic, RTopic, Bs) of
        {succeed, NewBs} ->
            ?INFO_MSG("Unified", []),

            FailFun =
                fun(#cp{next = NextF,
                        bs = Bs0,
                        vn = Vnf},
                    LCps,
                    Lst) ->
                   mqtt_subscribed_predicate_browse(DTopic,
                                                    DClientPid,
                                                    NextF,
                                                    Lst#est{cps = LCps,
                                                            bs = Bs0,
                                                            vn = Vnf + 1},
                                                    OtherTopics)
                end,
            Cp = #cp{type = compiled,
                     data = FailFun,
                     next = Next0,
                     bs = Bs,
                     vn = Vn},
            erlog_int:prove_body(Next0, St#est{bs = NewBs, cps = [Cp | Cps]});
        _ ->
            ?INFO_MSG("NOT unified", []),
            erlog_int:fail(St)
    end.

mqtt_subscribe_predicate({_, Client, Topic}, Next0, #est{bs = Bs} = St) ->
    Vars = [DClient, DTopic] = erlog_int:dderef([Client, Topic], Bs),
    case erlog:vars_in(Vars) of
        [] ->
            case emqtt:subscribe(DClient, #{}, [{DTopic, [{nl, 1}]}]) of
                {ok, _Properties, _ReasonCodes} ->
                    ?INFO_MSG("Subscribed results ~p   ~p", [_Properties, _ReasonCodes]),
                    erlog_int:prove_body(Next0, St);
                {error, Reason} ->
                    ?INFO_MSG("Failled subscription reason ~p ", [Reason]),
                    erlog_int:fail(St)
            end;
        Unbinded ->
            ?INFO_MSG("Publish call with unbinded ~p ", [Unbinded]),
            erlog_int:fail(St)
    end.

mqtt_publish_predicate({_, Client, Topic, Payload, Options}, Next0, #est{bs = Bs} = St) ->
    Vars = [DClient, DTopic, DPayload] = erlog_int:dderef([Client, Topic, Payload], Bs),
    ?INFO_MSG("--------> Published ~p    ~p    ~p", [Client, DTopic, DPayload]),

    OptionsFinal =
        case Options of
            OptionsList when is_list(OptionsList) ->
                OptionsList;
            _ ->
                []
        end,

    case erlog:vars_in(Vars) of
        [] ->
            case emqtt:publish(DClient, DTopic, #{}, DPayload, OptionsFinal) of
                ok ->
                    erlog_int:prove_body(Next0, St);
                {ok, _PacketId} ->
                    erlog_int:prove_body(Next0, St);
                {error, Reason} ->
                    ?INFO_MSG("Failled publish reason ~p ", [Reason]),
                    erlog_int:fail(St)
            end;
        Unbinded ->
            ?INFO_MSG("Publish call with unbinded ~p ", [Unbinded]),
            erlog_int:fail(St)
    end.

connect_async_predicate({_, {_}, {_}}, _, St) ->
    ?INFO_MSG("--- >Connect mqtt FAIL", []),
    erlog_int:fail(St);
connect_async_predicate({_, OptionsList, ClientID}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("-------> Connect mqtt", []),
    DOptionList = erlog_int:dderef(OptionsList, Bs),
    case erlog:vars_in(DOptionList) of
        [] ->
            ?INFO_MSG("-------> Continue 1", []),
            AgentPid = self(),
            %% All options are binded, we continue
            erlang:spawn(?MODULE, perform_connect, [DOptionList, ClientID, AgentPid]),
            erlog_int:prove_body(Next0, St);
        _ ->
            erlog_int:fail(St)
    end.

perform_connect(OptionsList, AgentName, ParentPid) ->
    Options2 = lists:keymerge(1, [{clientid, AgentName}, {owner, ParentPid}], OptionsList),
    ?INFO_MSG("-------> Keylist ~p", [Options2]),

    case gen_statem:start(emqtt, [Options2], []) of
        {ok, Pid} ->
            Host = proplists:get_value(host, Options2, "localhost"),
            Port = proplists:get_value(port, Options2, 5748),
            ParentPid ! {mqtt_client_connected, Pid, Host, Port},
            ok;
        {error, _Error} ->
            ?ERROR_MSG("------->Error connectin mqtt client ~p", [_Error]),

            ok
    end.

% TBD
% connected_predicate({_, BrokerHost, ClientID}, Next0, #est{bs = Bs} = St) ->
%     DBrokerHost = erlog_int:dderef(BrokerHost, Bs),
%     case emqtt:connected() of
%         true -> ok
%     end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Subscribe Agent to topic AgenName/AccName/Ontology
%% If CcName is not binded, it will be binded to a uuid4
%% @end
%%------------------------------------------------------------------------------

new_cc_predicate({_, CcName, Ontology}, Next0, #est{bs = Bs} = St) ->
    BCcName = erlog_int:dderef(CcName, Bs),
    COnt = erlog_int:dderef(Ontology, Bs),
    process_new_cc(BCcName, COnt, Next0, St).

process_new_cc({_} = VarCc, Bontology, Next0, St) ->
    Me = get(agent_name),
    CCName = zuuid:v4(),
    case Bontology of
        OntName when is_binary(OntName) ->
            emqx:subscribe(iolist_to_binary([Me, <<"/">>, CCName, <<"/">>, OntName])),
            erlog_int:unify_prove_body(VarCc, CCName, Next0, St);
        _ ->
            erlog_int:fail(St)
    end;
process_new_cc(VarCc, Bontology, Next0, St) when is_binary(VarCc) ->
    Me = get(agent_name),
    CCName = VarCc,
    case Bontology of
        OntName when is_binary(OntName) ->
            emqx:subscribe(iolist_to_binary([Me, <<"/">>, CCName, <<"/">>, OntName])),
            erlog_int:unify_prove_body(VarCc, CCName, Next0, St);
        _ ->
            erlog_int:fail(St)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Send provided mqtt message on provided topic. Both needs to be binded.
%%
%% @end
%%------------------------------------------------------------------------------

mqtt_send_predicate({_, CcId, Predicate}, Next0, #est{bs = Bs} = St) ->
    lager:info("SENGINNG", []),
    DCCiD = erlog_int:dderef(CcId, Bs),
    DPred = erlog_int:dderef(Predicate, Bs),
    case do_mqtt_send(DCCiD, DPred) of
        ok ->
            erlog_int:prove_body(Next0, St);
        _ ->
            erlog_int:fail(St)
    end.

%% Can't post to an unbinded topic
do_mqtt_send({_}, _Predicate) ->
    fail;
%% Can't send an undefined message ... for now
do_mqtt_send(_, {_}) ->
    fail;
%% We can send the message
do_mqtt_send(Topic, Predicate) ->
    %% This is not super good. Ideally From should be fetched at a higher level querying "bbs:agent"::me(Me), but doing
    %% this here permits so avoid client faking its From too easily
    Me = get(agent_name),
    %% Let's send.
    emqx:publish(#{topic => Topic,
                   from => Me,
                   payload => Predicate}),
    ok.

topic_ccid_predicate({_, Topic, CcId}, Next0, #est{bs = Bs} = St) ->
    DCCiD = erlog_int:dderef(CcId, Bs),
    DTopic = erlog_int:dderef(Topic, Bs),
    do_topic_ccid(DTopic, DCCiD, Next0, St).

do_topic_ccid({_}, {_}, _Next0, #est{} = St) ->
    erlog_int:fail(St);
do_topic_ccid({_}, CcId, _Next0, #est{} = St) when is_binary(CcId) ->
    erlog_int:fail(St);
do_topic_ccid(Topic, {_} = CcId, Next0, #est{} = St) when is_binary(Topic) ->
    erlog_int:unify_prove_body(CcId, toptic2ccid(Topic), Next0, St);
do_topic_ccid(Topic, CcId, Next0, #est{} = St)
    when is_binary(Topic) andalso is_binary(CcId) ->
    CcIdTopic = toptic2ccid(Topic),
    case CcIdTopic of
        CcId ->
            erlog_int:prove_body(Next0, Topic);
        _ ->
            erlog_int:fail(St)
    end.

toptic2ccid(Topic) ->
    Split = binary:split(Topic, <<"/">>, [global]),
    lists:nth(2, Split).
