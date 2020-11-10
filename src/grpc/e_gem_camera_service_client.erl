%%%-------------------------------------------------------------------
%% @doc Client module for grpc service eGem.camera_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_camera_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'eGem.camera_service').
-define(PROTO_MODULE, 'camera_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec add_camera_component(camera_service_pb:camera_component_parameters()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_camera_component(Input) ->
    add_camera_component(ctx:new(), Input, #{}).

-spec add_camera_component(ctx:t() | camera_service_pb:camera_component_parameters(), camera_service_pb:camera_component_parameters() | grpcbox_client:options()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_camera_component(Ctx, Input) when ?is_ctx(Ctx) ->
    add_camera_component(Ctx, Input, #{});
add_camera_component(Input, Options) ->
    add_camera_component(ctx:new(), Input, Options).

-spec add_camera_component(ctx:t(), camera_service_pb:camera_component_parameters(), grpcbox_client:options()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
add_camera_component(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/eGem.camera_service/add_camera_component">>, Input, ?DEF(camera_component_parameters, generic_result_code, <<"eGem.camera_component_parameters">>), Options).

%% @doc Unary RPC
-spec activate_camera_entity(camera_service_pb:m_entity_id()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
activate_camera_entity(Input) ->
    activate_camera_entity(ctx:new(), Input, #{}).

-spec activate_camera_entity(ctx:t() | camera_service_pb:m_entity_id(), camera_service_pb:m_entity_id() | grpcbox_client:options()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
activate_camera_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    activate_camera_entity(Ctx, Input, #{});
activate_camera_entity(Input, Options) ->
    activate_camera_entity(ctx:new(), Input, Options).

-spec activate_camera_entity(ctx:t(), camera_service_pb:m_entity_id(), grpcbox_client:options()) ->
    {ok, camera_service_pb:generic_result_code(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
activate_camera_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/eGem.camera_service/activate_camera_entity">>, Input, ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>), Options).

