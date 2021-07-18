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

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

-define(DEFAULT_TIMEOUT, 60 * 1000).
%% Prolog API

-define(ERLANG_PREDS,
    [
        {{connect, 3}, ?MODULE, connect_predicate},
        {{connect_async, 2}, ?MODULE, connect_async_predicate},
        {{mqtt_publish, 3}, ?MODULE, mqtt_publish_predicate},
        {{mqtt_subscribe, 2}, ?MODULE, mqtt_subscribe_predicate},
        {{new_cc, 2}, ?MODULE, new_cc_predicate},
        {{mqtt_send, 2}, ?MODULE, mqtt_send_predicate},
        {{topic_ccid, 2}, ?MODULE, topic_ccid_predicate}]).

%% API
-export([external_predicates/0]).
-export([perform_connect/3, msg_handler/2, disconnected/1]).
-export([connect_predicate/3, connect_async_predicate/3]).
-export([mqtt_subscribe_predicate/3, mqtt_publish_predicate/3]).

-export([new_cc_predicate/3, mqtt_send_predicate/3, topic_ccid_predicate/3]).

external_predicates() ->
    ?ERLANG_PREDS.


%new_client_predicate({_, Host, Port, PidClientOut}, Next0, #est{bs = Bs} = St) ->




connect_predicate({_, OptionsList, ClientID, PidClientOut}, Next0, #est{bs = Bs} = St) ->
    DOptionList = erlog_int:dderef(OptionsList, Bs),
    DClientID = erlog_int:dderef(ClientID, Bs),
    DPidClientOut = erlog_int:dderef(PidClientOut, Bs),
    case erlog:vars_in(DOptionList) of
        [] ->
            AgentPid = self(),
            ClientIdEntry = case DClientID of
                                {_} -> [];
                                _ ->  [{clientid, DClientID}]
                            end,
            Options2 = lists:keymerge(1, [
                {msg_handler, #{disconnected => fun disconnected/1, publish => {?MODULE, msg_handler,[AgentPid]}}},
                {owner, AgentPid}] ++ ClientIdEntry, OptionsList),

            case gen_statem:start(emqtt, [Options2], []) of
                {ok, PidClient} ->
                    case emqtt:connect(PidClient) of
                        {ok, _Props} ->
                            FinalClientID = proplists:get_value(clientid, gen_statem:call(PidClient, info)),
                            case erlog_int:unify(FinalClientID, DClientID, Bs) of
                                {succeed, NewBs} ->
                                    %% This should always be true
                                    ?INFO_MSG("Client connected ~p",[FinalClientID]),
                                    erlog_int:unify_prove_body(DPidClientOut, PidClient, Next0, St#est{bs = NewBs});
                                _ ->
                                    emqtt:disconnect(PidClient),
                                    erlog_int:fail(St)
                            end;
                        {error, Reason} ->
                            ?INFO_MSG("Error connecting mqtt client ~p",[Reason]),
                            erlog_int:fail(St)
                    end;
                {error, _Error} ->
                    ?INFO_MSG("Error strating mqtt client ~p",[_Error]),
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end.

msg_handler(Msg, Pid) ->
    ?INFO_MSG("Processing publish ~p to ~p",[Msg, Pid]),
    Pid!{incoming_mqtt_message,
        maps:get(qos, Msg), maps:get(dup, Msg), maps:get(retain, Msg), maps:get(packet_id, Msg),
        maps:get(topic, Msg), maps:get(payload, Msg)}.

disconnected(Data) ->
    ?INFO_MSG("DISCONNECTED ~p",[Data]).

mqtt_subscribe_predicate({_, Client, Topic}, Next0, #est{bs = Bs} = St) ->
    Vars = [DClient, DTopic] = erlog_int:dderef([Client, Topic], Bs),
    case erlog:vars_in(Vars) of
        [] ->
            case emqtt:subscribe(DClient, #{}, [{DTopic, [{nl, 1}]}]) of
                {ok, _Properties, _ReasonCodes} ->
                    ?INFO_MSG("Subscribed results ~p   ~p",[_Properties, _ReasonCodes]),
                    erlog_int:prove_body(Next0,St);
                {error, Reason} ->
                    ?INFO_MSG("Failled subscription reason ~p ",[Reason]),
                    erlog_int:fail(St)
            end;
        Unbinded ->
            ?INFO_MSG("Publish call with unbinded ~p ",[Unbinded]),
            erlog_int:fail(St)
    end.

mqtt_publish_predicate({_, Client, Topic, Payload}, Next0, #est{bs = Bs} = St) ->
    Vars = [DClient, DTopic, DPayload] = erlog_int:dderef([Client, Topic, Payload], Bs),
    case erlog:vars_in(Vars) of
        [] ->
            case emqtt:publish(DClient, DTopic, [], DPayload, []) of
                {ok, _PacketId} ->
                    erlog_int:prove_body(Next0, St);
                {error, Reason} ->
                    ?INFO_MSG("Failled publish reason ~p ",[Reason]),
                    erlog_int:fail(St)
            end ;
        Unbinded ->
            ?INFO_MSG("Publish call with unbinded ~p ",[Unbinded]),
            erlog_int:fail(St)
    end.


connect_async_predicate({_, {_}, {_}}, _, St) ->
    ?INFO_MSG("--- >Connect mqtt FAIL",[]),
    erlog_int:fail(St);

connect_async_predicate({_, OptionsList, ClientID}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("-------> Connect mqtt",[]),
    DOptionList = erlog_int:dderef(OptionsList, Bs),
    case erlog:vars_in(DOptionList) of
        [] ->
            ?INFO_MSG("-------> Continue 1",[]),
            AgentPid = self(),
            %% All options are binded, we continue
            erlang:spawn(?MODULE, perform_connect, [DOptionList, ClientID, AgentPid]),
            erlog_int:prove_body(Next0, St);

        _ ->
            erlog_int:fail(St)
    end.

perform_connect(OptionsList, AgentName, ParentPid) ->
    Options2 = lists:keymerge(1, [{clientid, AgentName}, {owner, ParentPid}], OptionsList),
    ?INFO_MSG("-------> Keylist ~p",[Options2]),

    case gen_statem:start(emqtt, [Options2], []) of
        {ok, Pid} ->
            Host = proplists:get_value(host, Options2, "localhost"),
            Port = proplists:get_value(port, Options2, 5748),
            ParentPid!{mqtt_client_connected, Pid, Host, Port},
            ok;
        {error, _Error} ->
            ?ERROR_MSG("------->Error connectin mqtt client ~p",[_Error]),

            ok
    end.
connected_predicate({_, BrokerHost, ClientID}, Next0, #est{bs = Bs} = St) ->
    DBrokerHost = erlog_int:dderef(BrokerHost, Bs),
    case emqtt:connected() of
        true -> ok
    end.

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
