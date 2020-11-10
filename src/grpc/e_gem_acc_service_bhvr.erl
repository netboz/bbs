%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service eGem.acc_service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-07-29T10:50:14+00:00 and should not be modified manually

-module(e_gem_acc_service_bhvr).

%% @doc 
-callback send_predicate(reference(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

