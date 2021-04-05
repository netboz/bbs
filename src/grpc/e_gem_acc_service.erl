%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2020 11:42 PM
%%%-------------------------------------------------------------------
-module(e_gem_acc_service).

-author("yan").

-behaviour(e_gem_acc_service_bhvr).

%% API
-export([send_predicate/2]).

send_predicate(Ref, StreamOut) ->
    lager:info("Running loop predicate :~p", [{Ref, StreamOut}]),
    receive
        {Ref, eos} ->
            lager:info("Received endof predicate stream", []),
            ok;
        {Ref, InPredicate} ->
            lager:info("Received predicate :~p", [{Ref, InPredicate}]),
            %% TODO: Decode, validate and send predicate to agent
            grpcbox_stream:send(#{query_result => <<"Predres">>, metadata => <<"metadata">>},
                                StreamOut),
            send_predicate(Ref, StreamOut);
        {out_predicate, OutPredicate} ->
            %% TODO : Validate and encode predicate
            lager:info("sending_predicate :~p", [OutPredicate]),
            grpcbox_stream:send(OutPredicate, StreamOut),
            send_predicate(ref, StreamOut);
        {out_predicate_result, OutPredicateResult} ->
            %% TODO : Validate and encode predicate result
            lager:info("sending_predicate result: ~p", [OutPredicateResult]),
            grpcbox_stream:send(OutPredicateResult, StreamOut),
            send_predicate(ref, StreamOut);
        Else ->
            lager:info("Received unamanged message :~p", [Else]),
            send_predicate(ref, StreamOut)
    end.
