%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2021 10:16 PM
%%%-------------------------------------------------------------------
-module(ont_bbs_mts_client_registry).

-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API
-define(NAMESPACE, <<"bbs:mts:client:registry">>).
-define(ERLANG_PREDS, [
    {{key, 1}, ?MODULE, key_predicate},
    {{aid, 2}, ?MODULE, aid_predicate},
    {{new_registry_entry, 2}, ?MODULE, new_registry_entry_predicate},
    {{update_registry_entry, 2}, ?MODULE, update_registry_entry_predicate},
    {{subscribe, 1}, ?MODULE, subsribe_predicate},
    {{cc, 4}, ?MODULE, cc_predicate},
    {{send, 2}, ?MODULE, send_predicate}
]).

%% API
-export([external_predicates/0]).
-export([
    key_predicate/3,
    aid_predicate/3,
    update_registry_entry_predicate/3,
    new_registry_entry_predicate/3,
    subsribe_predicate/3,
    send_predicate/3,
    cc_predicate/3
]).

external_predicates() ->
    ?ERLANG_PREDS.

subsribe_predicate({_, CcId}, Next0, #est{bs = Bs} = St) ->
    case erlog_int:dderef(CcId, Bs) of
        {_} ->
            erlog_int:fail(St);
        DccId ->
            case
                ?HORDEREG:register(
                    ?BBS_BUBBLES_REG,
                    {cc, DccId, get(agent_name), get(tree_node)},
                    ?NAMESPACE
                )
            of
                {ok, _} ->
                    erlog_int:prove_body(Next0, St);
                Error ->
                    ?WARNING_MSG("Failled CC subscription to ~p  reason :~p", [DccId, Error]),
                    erlog_int:fail(St)
            end
    end.

send_predicate({_, CcId, Payload}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("Sending  :~p", [Payload]),
    case erlog_int:dderef(CcId, Bs) of
        {_} ->
            erlog_int:fail(St);
        DCcId ->
            case erlog_int:dderef(Payload, Bs) of
                {_} ->
                    erlog_int:fail(St);
                DPayload ->
                    lists:foreach(
                        fun(Pid) ->
                            ?INFO_MSG("Sending to pid :~p", [Pid]),
                            Pid ! {registry_delivery, DPayload}
                        end,
                        ?HORDEREG:select(
                            ?BBS_BUBBLES_REG,
                            [{{{cc, DCcId, '_', '_'}, '$1', '_'}, [], ['$1']}]
                        )
                    ),
                    erlog_int:prove_body(Next0, St)
            end
    end.

cc_predicate({_, CcId, AgName, TreeNode, TransportOntology}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("--->Looking for CC :~p", [{CcId, AgName, TreeNode, TransportOntology}]),
    case erlog_int:dderef([CcId, AgName, TreeNode, TransportOntology], Bs) of
        [DCcId, DAg, DTree, DOnt] ->
            A = query_param(1, DCcId),
            B = query_param(2, DAg),
            C = query_param(3, DTree),
            D = query_param(4, DOnt),
            SelectedCCs =
                ?HORDEREG:select(
                    ?BBS_BUBBLES_REG,
                    [{{{cc, A, B, C}, '_', D}, [], [{{A, B, C, D}}]}]
                ),
            ?INFO_MSG("Selected CCs :~p", [SelectedCCs]),
            cc_predicate2(DCcId, DAg, DTree, DOnt, Next0, St, SelectedCCs)
    end.

cc_predicate2(_DCcId, _DAg, _DTree, _Dont, _Next0, St, []) ->
    erlog_int:fail(St);
cc_predicate2(
    DCcId,
    DAg,
    DTree,
    DOnt,
    Next0,
    #est{
        bs = Bs,
        vn = Vn,
        cps = Cps
    } =
        St,
    [{RCcId, RAg, RTree, ROnt} | Tail]
) ->
    ?INFO_MSG("cc_predicate2", []),

    case unify_cc([{DCcId, RCcId}, {DAg, RAg}, {DTree, RTree}, {DOnt, ROnt}], Bs) of
        {succeed, NewBs} ->
            ?INFO_MSG("Unified", []),

            FailFun =
                fun(
                    #cp{
                        next = NextF,
                        bs = Bs0,
                        vn = Vnf
                    },
                    LCps,
                    Lst
                ) ->
                    cc_predicate2(
                        DCcId,
                        DAg,
                        DTree,
                        DOnt,
                        NextF,
                        Lst#est{
                            cps = LCps,
                            bs = Bs0,
                            vn = Vnf + 4
                        },
                        Tail
                    )
                end,
            Cp = #cp{
                type = compiled,
                data = FailFun,
                next = Next0,
                bs = Bs,
                vn = Vn
            },
            erlog_int:prove_body(Next0, St#est{bs = NewBs, cps = [Cp | Cps]});
        _ ->
            ?INFO_MSG("NOT unified", []),
            erlog_int:fail(St)
    end.

key_predicate({_, Key}, Next0, #est{bs = Bs} = St) ->
    DKey = erlog_int:dderef(Key, Bs),
    PKey = query_param(1, DKey),
    SelectedKeys =
        ?HORDEREG:select(?BBS_BUBBLES_REG, [{{{aid, PKey}, '_', '_'}, [], [PKey]}]),
    ?INFO_MSG("Selected Aids :~p", [SelectedKeys]),
    key_predicate2(DKey, Next0, St, SelectedKeys).

key_predicate2(_DKey, _Next0, St, []) ->
    erlog_int:fail(St);
key_predicate2(DKey, Next0, #est{bs = Bs, vn = Vn, cps = Cps} = St, [RKey | OtherKeys]) ->
    case erlog_int:unify(DKey, RKey, Bs) of
        {succeed, NewBs} ->
            FailFun =
                fun(
                    #cp{
                        next = NextF,
                        bs = Bs0,
                        vn = Vnf
                    },
                    LCps,
                    Lst
                ) ->
                    key_predicate2(
                        DKey,
                        NextF,
                        Lst#est{
                            cps = LCps,
                            bs = Bs0,
                            vn = Vnf + 1
                        },
                        OtherKeys
                    )
                end,
            Cp = #cp{
                type = compiled,
                data = FailFun,
                next = Next0,
                bs = Bs,
                vn = Vn
            },
            erlog_int:prove_body(Next0, St#est{bs = NewBs, cps = [Cp | Cps]});
        _ ->
            erlog_int:fail(St)
    end.

aid_predicate({_, Key, Value}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("AID Pred ~p   ~p", [Key, Value]),
    [DKey, DVal] = erlog_int:dderef([Key, Value], Bs),
    PKey = query_param(1, DKey),
    PVal = query_param(2, DVal),
    SelectedAids =
        ?HORDEREG:select(?BBS_BUBBLES_REG, [{{{aid, PKey}, '_', PVal}, [], [{{PKey, PVal}}]}]),
    ?INFO_MSG("Selected Aids :~p", [SelectedAids]),
    aid_predicate2(DKey, DVal, Next0, St, SelectedAids).

aid_predicate2(_DKey, _DVal, _Next0, St, []) ->
    erlog_int:fail(St);
aid_predicate2(
    DKey,
    DVal,
    Next0,
    #est{
        bs = Bs,
        vn = Vn,
        cps = Cps
    } =
        St,
    [{RKey, RVal} | Tail]
) ->
    ?INFO_MSG("Browsing  :~p", [[{RKey, RVal} | Tail]]),

    case erlog_int:unify(DKey, RKey, Bs) of
        {succeed, NewBs} ->
            case erlog_int:unify(DVal, RVal, NewBs) of
                {succeed, NewBs2} ->
                    FailFun =
                        fun(
                            #cp{
                                next = NextF,
                                bs = Bs0,
                                vn = Vnf
                            },
                            LCps,
                            Lst
                        ) ->
                            aid_predicate2(
                                DKey,
                                DVal,
                                NextF,
                                Lst#est{
                                    cps = LCps,
                                    bs = Bs0,
                                    vn = Vnf + 2
                                },
                                Tail
                            )
                        end,
                    Cp = #cp{
                        type = compiled,
                        data = FailFun,
                        next = Next0,
                        bs = Bs,
                        vn = Vn
                    },
                    erlog_int:prove_body(Next0, St#est{bs = NewBs2, cps = [Cp | Cps]});
                _ ->
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end.

new_registry_entry_predicate({_, Key, Value}, Next0, #est{bs = Bs} = St) ->
    [DKey, DValue] = erlog_int:dderef([Key, Value], Bs),
    case erlog:vars_in([DKey, DValue]) of
        [] ->
            case ?HORDEREG:register(?BBS_BUBBLES_REG, {aid, Key}, Value) of
                {ok, _} ->
                    erlog_int:prove_body(Next0, St);
                Error ->
                    ?WARNING_MSG("Failled Adding aid : ~p   ~p   error:~p", [DKey, DValue, Error]),
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end.

update_registry_entry_predicate({_, Key, Value}, Next0, #est{bs = Bs} = St) ->
    [DKey, DValue] = erlog_int:dderef([Key, Value], Bs),
    case erlog:vars_in([DKey, DValue]) of
        [] ->
            case
                ?HORDEREG:update_value(?BBS_BUBBLES_REG, {aid, DKey}, fun(_Oldval) -> DValue end)
            of
                {_, _} ->
                    erlog_int:prove_body(Next0, St);
                Other ->
                    ?WARNING_MSG("Failled to update aid  :~p   error :~p", [{DKey, DValue}, Other]),
                    erlog_int:fail(St)
            end;
        _ ->
            erlog_int:fail(St)
    end.

query_param(1, {_}) ->
    '$1';
query_param(1, Value) ->
    Value;
query_param(2, {_}) ->
    '$2';
query_param(2, Value) ->
    Value;
query_param(3, {_}) ->
    '$3';
query_param(3, Value) ->
    Value;
query_param(4, {_}) ->
    '$4';
query_param(4, Value) ->
    Value.

unify_cc([], Bs) ->
    {succeed, Bs};
unify_cc([{D, R} | Tail], Bs) ->
    case erlog_int:unify(D, R, Bs) of
        {succeed, NewBs} ->
            unify_cc(Tail, NewBs);
        fail ->
            fail
    end.
