%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service eGem.shape_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_shape_service_bhvr).

%% @doc Unary RPC
-callback add_shape_component(ctx:ctx(), shape_service_pb:add_shape_component_parameters()) ->
    {ok, shape_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback remove_shape_components(ctx:ctx(), shape_service_pb:m_entity_id()) ->
    {ok, shape_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

