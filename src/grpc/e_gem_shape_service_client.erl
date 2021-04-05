%%%-------------------------------------------------------------------
%% @doc Client module for grpc service eGem.shape_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_shape_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).
-define(SERVICE, 'eGem.shape_service').
-define(PROTO_MODULE, shape_service_pb).
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType),
        #grpcbox_def{service = ?SERVICE,
                     message_type = MessageType,
                     marshal_fun = ?MARSHAL_FUN(Input),
                     unmarshal_fun = ?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec add_shape_component(shape_service_pb:add_shape_component_parameters()) ->
                             {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_shape_component(Input) ->
    add_shape_component(ctx:new(), Input, #{}).

-spec add_shape_component(ctx:t() | shape_service_pb:add_shape_component_parameters(),
                          shape_service_pb:add_shape_component_parameters() |
                          grpcbox_client:options()) ->
                             {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_shape_component(Ctx, Input) when ?is_ctx(Ctx) ->
    add_shape_component(Ctx, Input, #{});
add_shape_component(Input, Options) ->
    add_shape_component(ctx:new(), Input, Options).

-spec add_shape_component(ctx:t(),
                          shape_service_pb:add_shape_component_parameters(),
                          grpcbox_client:options()) ->
                             {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_shape_component(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.shape_service/add_shape_component">>,
                         Input,
                         ?DEF(add_shape_component_parameters,
                              generic_result_code,
                              <<"eGem.add_shape_component_parameters">>),
                         Options).

%% @doc Unary RPC
-spec remove_shape_components(shape_service_pb:m_entity_id()) ->
                                 {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_shape_components(Input) ->
    remove_shape_components(ctx:new(), Input, #{}).

-spec remove_shape_components(ctx:t() | shape_service_pb:m_entity_id(),
                              shape_service_pb:m_entity_id() | grpcbox_client:options()) ->
                                 {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_shape_components(Ctx, Input) when ?is_ctx(Ctx) ->
    remove_shape_components(Ctx, Input, #{});
remove_shape_components(Input, Options) ->
    remove_shape_components(ctx:new(), Input, Options).

-spec remove_shape_components(ctx:t(),
                              shape_service_pb:m_entity_id(),
                              grpcbox_client:options()) ->
                                 {ok, shape_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_shape_components(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.shape_service/remove_shape_components">>,
                         Input,
                         ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>),
                         Options).
