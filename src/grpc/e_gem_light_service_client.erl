%%%-------------------------------------------------------------------
%% @doc Client module for grpc service eGem.light_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_light_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).
-define(SERVICE, 'eGem.light_service').
-define(PROTO_MODULE, light_service_pb).
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType),
        #grpcbox_def{service = ?SERVICE,
                     message_type = MessageType,
                     marshal_fun = ?MARSHAL_FUN(Input),
                     unmarshal_fun = ?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec add_light_component(light_service_pb:light_component_parameters()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_light_component(Input) ->
    add_light_component(ctx:new(), Input, #{}).

-spec add_light_component(ctx:t() | light_service_pb:light_component_parameters(),
                          light_service_pb:light_component_parameters() |
                          grpcbox_client:options()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_light_component(Ctx, Input) when ?is_ctx(Ctx) ->
    add_light_component(Ctx, Input, #{});
add_light_component(Input, Options) ->
    add_light_component(ctx:new(), Input, Options).

-spec add_light_component(ctx:t(),
                          light_service_pb:light_component_parameters(),
                          grpcbox_client:options()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
add_light_component(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.light_service/add_light_component">>,
                         Input,
                         ?DEF(light_component_parameters,
                              generic_result_code,
                              <<"eGem.light_component_parameters">>),
                         Options).

%% @doc Unary RPC
-spec remove_light_components(light_service_pb:m_entity_id()) ->
                                 {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_light_components(Input) ->
    remove_light_components(ctx:new(), Input, #{}).

-spec remove_light_components(ctx:t() | light_service_pb:m_entity_id(),
                              light_service_pb:m_entity_id() | grpcbox_client:options()) ->
                                 {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_light_components(Ctx, Input) when ?is_ctx(Ctx) ->
    remove_light_components(Ctx, Input, #{});
remove_light_components(Input, Options) ->
    remove_light_components(ctx:new(), Input, Options).

-spec remove_light_components(ctx:t(),
                              light_service_pb:m_entity_id(),
                              grpcbox_client:options()) ->
                                 {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                                 grpcbox_stream:grpc_error_response() |
                                 {error, any()}.
remove_light_components(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.light_service/remove_light_components">>,
                         Input,
                         ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>),
                         Options).

%% @doc Unary RPC
-spec update_light_status(light_service_pb:light_component_parameters()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
update_light_status(Input) ->
    update_light_status(ctx:new(), Input, #{}).

-spec update_light_status(ctx:t() | light_service_pb:light_component_parameters(),
                          light_service_pb:light_component_parameters() |
                          grpcbox_client:options()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
update_light_status(Ctx, Input) when ?is_ctx(Ctx) ->
    update_light_status(Ctx, Input, #{});
update_light_status(Input, Options) ->
    update_light_status(ctx:new(), Input, Options).

-spec update_light_status(ctx:t(),
                          light_service_pb:light_component_parameters(),
                          grpcbox_client:options()) ->
                             {ok, light_service_pb:generic_result_code(), grpcbox:metadata()} |
                             grpcbox_stream:grpc_error_response() |
                             {error, any()}.
update_light_status(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.light_service/update_light_status">>,
                         Input,
                         ?DEF(light_component_parameters,
                              generic_result_code,
                              <<"eGem.light_component_parameters">>),
                         Options).
