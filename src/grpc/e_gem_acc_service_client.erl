%%%-------------------------------------------------------------------
%% @doc Client module for grpc service eGem.acc_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-07-29T10:50:14+00:00 and should not be modified manually

-module(e_gem_acc_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'eGem.acc_service').
-define(PROTO_MODULE, 'acc_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc 
-spec send_predicate() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
send_predicate() ->
    send_predicate(ctx:new(), #{}).

-spec send_predicate(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
send_predicate(Ctx) when ?is_ctx(Ctx) ->
    send_predicate(Ctx, #{});
send_predicate(Options) ->
    send_predicate(ctx:new(), Options).

-spec send_predicate(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
send_predicate(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/eGem.acc_service/send_predicate">>, ?DEF(predicate, predicate_result, <<"eGem.predicate">>), Options).

