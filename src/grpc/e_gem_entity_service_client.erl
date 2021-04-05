%%%-------------------------------------------------------------------
%% @doc Client module for grpc service eGem.entity_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_entity_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).
-define(SERVICE, 'eGem.entity_service').
-define(PROTO_MODULE, entity_service_pb).
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType),
        #grpcbox_def{service = ?SERVICE,
                     message_type = MessageType,
                     marshal_fun = ?MARSHAL_FUN(Input),
                     unmarshal_fun = ?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec create_entity(entity_service_pb:create_entity_params()) ->
                       {ok, entity_service_pb:entity_creation_result(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
create_entity(Input) ->
    create_entity(ctx:new(), Input, #{}).

-spec create_entity(ctx:t() | entity_service_pb:create_entity_params(),
                    entity_service_pb:create_entity_params() | grpcbox_client:options()) ->
                       {ok, entity_service_pb:entity_creation_result(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
create_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    create_entity(Ctx, Input, #{});
create_entity(Input, Options) ->
    create_entity(ctx:new(), Input, Options).

-spec create_entity(ctx:t(),
                    entity_service_pb:create_entity_params(),
                    grpcbox_client:options()) ->
                       {ok, entity_service_pb:entity_creation_result(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
create_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.entity_service/create_entity">>,
                         Input,
                         ?DEF(create_entity_params,
                              entity_creation_result,
                              <<"eGem.create_entity_params">>),
                         Options).

%% @doc Unary RPC
-spec activate_entity(entity_service_pb:m_entity_id()) ->
                         {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                         grpcbox_stream:grpc_error_response() |
                         {error, any()}.
activate_entity(Input) ->
    activate_entity(ctx:new(), Input, #{}).

-spec activate_entity(ctx:t() | entity_service_pb:m_entity_id(),
                      entity_service_pb:m_entity_id() | grpcbox_client:options()) ->
                         {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                         grpcbox_stream:grpc_error_response() |
                         {error, any()}.
activate_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    activate_entity(Ctx, Input, #{});
activate_entity(Input, Options) ->
    activate_entity(ctx:new(), Input, Options).

-spec activate_entity(ctx:t(),
                      entity_service_pb:m_entity_id(),
                      grpcbox_client:options()) ->
                         {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                         grpcbox_stream:grpc_error_response() |
                         {error, any()}.
activate_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.entity_service/activate_entity">>,
                         Input,
                         ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>),
                         Options).

%% @doc Unary RPC
-spec deactivate_entity(entity_service_pb:m_entity_id()) ->
                           {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                           grpcbox_stream:grpc_error_response() |
                           {error, any()}.
deactivate_entity(Input) ->
    deactivate_entity(ctx:new(), Input, #{}).

-spec deactivate_entity(ctx:t() | entity_service_pb:m_entity_id(),
                        entity_service_pb:m_entity_id() | grpcbox_client:options()) ->
                           {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                           grpcbox_stream:grpc_error_response() |
                           {error, any()}.
deactivate_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    deactivate_entity(Ctx, Input, #{});
deactivate_entity(Input, Options) ->
    deactivate_entity(ctx:new(), Input, Options).

-spec deactivate_entity(ctx:t(),
                        entity_service_pb:m_entity_id(),
                        grpcbox_client:options()) ->
                           {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                           grpcbox_stream:grpc_error_response() |
                           {error, any()}.
deactivate_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.entity_service/deactivate_entity">>,
                         Input,
                         ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>),
                         Options).

%% @doc Unary RPC
-spec delete_entity(entity_service_pb:m_entity_id()) ->
                       {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
delete_entity(Input) ->
    delete_entity(ctx:new(), Input, #{}).

-spec delete_entity(ctx:t() | entity_service_pb:m_entity_id(),
                    entity_service_pb:m_entity_id() | grpcbox_client:options()) ->
                       {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
delete_entity(Ctx, Input) when ?is_ctx(Ctx) ->
    delete_entity(Ctx, Input, #{});
delete_entity(Input, Options) ->
    delete_entity(ctx:new(), Input, Options).

-spec delete_entity(ctx:t(), entity_service_pb:m_entity_id(), grpcbox_client:options()) ->
                       {ok, entity_service_pb:generic_result_code(), grpcbox:metadata()} |
                       grpcbox_stream:grpc_error_response() |
                       {error, any()}.
delete_entity(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/eGem.entity_service/delete_entity">>,
                         Input,
                         ?DEF(m_entity_id, generic_result_code, <<"eGem.m_entity_id">>),
                         Options).
