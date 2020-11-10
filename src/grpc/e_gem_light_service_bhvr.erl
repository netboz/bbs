%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service eGem.light_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_light_service_bhvr).

%% @doc Unary RPC
-callback add_light_component(ctx:ctx(), light_service_pb:light_component_parameters()) ->
    {ok, light_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback remove_light_components(ctx:ctx(), light_service_pb:m_entity_id()) ->
    {ok, light_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback update_light_status(ctx:ctx(), light_service_pb:light_component_parameters()) ->
    {ok, light_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

