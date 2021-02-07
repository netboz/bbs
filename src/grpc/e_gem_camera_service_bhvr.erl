%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service eGem.camera_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-06-26T19:33:47+00:00 and should not be modified manually

-module(e_gem_camera_service_bhvr).

%% @doc Unary RPC
-callback add_camera_component(ctx:ctx(), camera_service_pb:camera_component_parameters()) ->
    {ok, camera_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback activate_camera_entity(ctx:ctx(), camera_service_pb:m_entity_id()) ->
    {ok, camera_service_pb:generic_result_code(), ctx:ctx()} | grpcbox_stream:grpc_error_response().
