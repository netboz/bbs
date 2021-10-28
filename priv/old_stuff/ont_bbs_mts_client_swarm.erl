%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2021 10:16 PM
%%%-------------------------------------------------------------------
-module('ont_bbs_mts_client_swarm.erl').

-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").


%% Prolog API

-define(ERLANG_PREDS,
        [{{join, 2}, ?MODULE, join_predicate},
         {{mqtt_send, 2}, ?MODULE, mqtt_send_predicate},
         {{topic_ccid, 2}, ?MODULE, topic_ccid_predicate}]).

%% API
-export([external_predicates/0]).
-export([new_cc_predicate/3, mqtt_send_predicate/3, topic_ccid_predicate/3]).

external_predicates() ->
    ?ERLANG_PREDS.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Subscribe Agent to topic AgentName/AccName/Ontology
%% If CcName is not binded, it will be binded to a uuid4
%% @end
%%------------------------------------------------------------------------------

join_predicate({_, CcName, Ontology}, Next0, #est{bs = Bs} = St) ->
    DCcName = erlog_int:dderef(CcName, Bs),
    DOntology = erlog_int:dderef(Ontology, Bs),
    process_new_cc(BCcName, COnt, Next0, St).

join_predicate({_} = VarCCName, DOntology, Next0, #est{bs = Bs} = St) ->
    Me = get(agent_name),
    CCName = zuuid:v4(),
    process_new_cc(VarCCName,DOntology, Next0, St#est{bs = erlog_int:add_binding()});

join_predicate(VarCCName, DOntology, Next0, St#est{bs = Bs}) ->
    swarm:
        swarm:members(iolist_to_binary([Me, <<"/">>, CCName, <<"/">>, OntName])),

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
    emqx:publish(#message{topic = Topic,
                          from = Me,
                          payload = Predicate}),
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
