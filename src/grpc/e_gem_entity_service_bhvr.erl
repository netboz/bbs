%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service eGem.entity_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_entity_service_bhvr).

%% @doc Unary RPC
-callback create_entity(ctx:ctx(), entity_service_pb:create_entity_params()) ->
                           {ok, entity_service_pb:entity_creation_result(), ctx:ctx()} |
                           grpcbox_stream:grpc_error_response().
%% @doc Unary RPC
-callback activate_entity(ctx:ctx(), entity_service_pb:m_entity_id()) ->
                             {ok, entity_service_pb:generic_result_code(), ctx:ctx()} |
                             grpcbox_stream:grpc_error_response().
%% @doc Unary RPC
-callback deactivate_entity(ctx:ctx(), entity_service_pb:m_entity_id()) ->
                               {ok, entity_service_pb:generic_result_code(), ctx:ctx()} |
                               grpcbox_stream:grpc_error_response().
%% @doc Unary RPC
-callback delete_entity(ctx:ctx(), entity_service_pb:m_entity_id()) ->
                           {ok, entity_service_pb:generic_result_code(), ctx:ctx()} |
                           grpcbox_stream:grpc_error_response().
