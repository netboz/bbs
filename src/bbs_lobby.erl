%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Feb 2020 18:11
%%%-------------------------------------------------------------------
-module(bbs_lobby).

-author("Yan").

-include("bbs.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, test/0, test_basic_scene/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(bbs_lobby_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: #bbs_lobby_state{}} |
              {ok, State :: #bbs_lobby_state{}, timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init([]) ->
    grpcbox:start_server(#{server_opts =>
                               #{header_table_size => 4096,
                                 enable_push => 1,
                                 max_concurrent_streams => unlimited,
                                 initial_window_size => 65535,
                                 max_frame_size => 16384,
                                 max_header_list_size => unlimited},
                           grpc_opts =>
                               #{service_protos => [acc_service_pb],
                                 services => #{'eGem.acc_service' => e_gem_acc_service}},
                           listen_opts => #{port => 7777, ip => {0, 0, 0, 0}},
                           pool_opts => #{size => 10},
                           transport_opts => #{ssl => false}}),
    {ok, #bbs_lobby_state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: #bbs_lobby_state{}) ->
                     {reply, Reply :: term(), NewState :: #bbs_lobby_state{}} |
                     {reply,
                      Reply :: term(),
                      NewState :: #bbs_lobby_state{},
                      timeout() | hibernate} |
                     {noreply, NewState :: #bbs_lobby_state{}} |
                     {noreply, NewState :: #bbs_lobby_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #bbs_lobby_state{}} |
                     {stop, Reason :: term(), NewState :: #bbs_lobby_state{}}.
handle_call(_Request, _From, State = #bbs_lobby_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #bbs_lobby_state{}) ->
                     {noreply, NewState :: #bbs_lobby_state{}} |
                     {noreply, NewState :: #bbs_lobby_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #bbs_lobby_state{}}.
handle_cast({new_bubble, #agent{} = BubbleSpecs}, State = #bbs_lobby_state{}) ->
    supervisor:start_child(bubbles_sup, [BubbleSpecs]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #bbs_lobby_state{}) ->
                     {noreply, NewState :: #bbs_lobby_state{}} |
                     {noreply, NewState :: #bbs_lobby_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #bbs_lobby_state{}}.
handle_info(_Info, State = #bbs_lobby_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #bbs_lobby_state{}) ->
                   term().
terminate(_Reason, _State = #bbs_lobby_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #bbs_lobby_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #bbs_lobby_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #bbs_lobby_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
test() ->
    timer:sleep(5000),
    {ok, #{entity_id := EntId}, _Headers} =
        pod_service_entity_service_client:create_entity(#{name => <<"entity1">>,
                                                          activate => true,
                                                          initial_position =>
                                                              #{x => 0,
                                                                y => 0,
                                                                z => 0}}),
    lager:info("Entity Id : ~p", [EntId]),
    pod_service_shape_component_service_client:add_shape_component(#{entity_id => EntId,
                                                                     type => primitive,
                                                                     parameters =>
                                                                         {primitive_parameters,
                                                                          #{primitive_type =>
                                                                                sphere,
                                                                            primitive_parameters =>
                                                                                {primitive_sphere_parameters,
                                                                                 #{radius =>
                                                                                       5}}}}}),
    lager:info("Adding camera"),
    pod_service_camera_service_client:add_camera_component(#{entity_id => EntId}),
    lager:info("activating entity"),

    pod_service_entity_service_client:activate_entity(EntId),

    lager:info("Creating second entity "),

    {ok, #{entity_id := EntId2}, _Headers} =
        pod_service_entity_service_client:create_entity(#{name => <<"entity2">>,
                                                          activate => true,
                                                          initial_position =>
                                                              #{x => 10,
                                                                y => 0,
                                                                z => 0}}),
    lager:info("Entity 2 Id : ~p", [EntId2]),
    pod_service_shape_component_service_client:add_shape_component(#{entity_id => EntId2,
                                                                     type => primitive,
                                                                     parameters =>
                                                                         {primitive_parameters,
                                                                          #{primitive_type =>
                                                                                sphere,
                                                                            primitive_parameters =>
                                                                                {primitive_sphere_parameters,
                                                                                 #{radius =>
                                                                                       4}}}}}),

    pod_service_camera_service_client:add_camera_component(#{entity_id => EntId2}),

    lager:info("activating camera 2 : ~p", [EntId2]),

    pod_service_camera_service_client:activate_camera_entity(EntId2),

    timer:sleep(10000),
    lager:info("Switching camera"),

    pod_service_camera_service_client:activate_camera_entity(EntId),

    lager:info("Test ended").

test_basic_scene() ->
    {ok, PlayerId, _Headers} =
        e_gem_entity_service_client:create_entity(#{name => <<"Bob">>,
                                                    activate => false,
                                                    initial_position =>
                                                        #{x => 1,
                                                          y => 1,
                                                          z => 1}}),

    PlayerCameraResult =
        egem_camera_service_client:add_camera_component(#{entity_id => PlayerId}),

    egem_entity_service_client:activate_entity(PlayerId),

    lager:info("Camera creation result :~p", [PlayerCameraResult]),

    {ok, #{entity_id := BulleId}, _Headers} =
        egem_entity_service_client:create_entity(#{name => <<"Bulle">>,
                                                   activate => true,
                                                   initial_position =>
                                                       #{x => 0,
                                                         y => 0,
                                                         z => 0}}),

    ResultBulleCreate =
        egem_shape_service_client:add_shape_component(#{entity_id => BulleId,
                                                        type => mesh,
                                                        parameters =>
                                                            {mesh_parameters,
                                                             #{mesh_source => asset_processor,
                                                               mesh_location =>
                                                                   "Gem/Assets/bulleglasstext1.fbx"}}}),
    lager:info("Bubble creation result :~p", [ResultBulleCreate]),

    {ok, #{entity_id := LightId}, _Headers} =
        egem_entity_service_client:create_entity(#{name => <<"Light">>,
                                                   activate => false,
                                                   initial_position =>
                                                       #{x => 0,
                                                         y => 10,
                                                         z => 0}}),

    lager:info("LightId :~p", [LightId]),

    LightCreationResult =
        egem_light_service_client:add_light_component(#{entity_id => LightId,
                                                        is_visible => true}),

    egem_entity_service_client:activate_entity(#{entity_id => LightId}),

    lager:info("Light creation result :~p", [LightCreationResult]),

    ActivationResult = egem_camera_service_client:activate_camera_entity(PlayerId),

    lager:info("Activation  result :~p", [ActivationResult]).
