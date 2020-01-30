%%%-------------------------------------------------------------------
%%% @author Yan
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2020 18:23
%%%-------------------------------------------------------------------
-module(bbs_port_manager).
-author("Yan").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(bbs_port_controler_state, {pipe_name, port_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #bbs_port_controler_state{}} | {ok, State :: #bbs_port_controler_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #bbs_port_controler_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #bbs_port_controler_state{}) ->
  {reply, Reply :: term(), NewState :: #bbs_port_controler_state{}} |
  {reply, Reply :: term(), NewState :: #bbs_port_controler_state{}, timeout() | hibernate} |
  {noreply, NewState :: #bbs_port_controler_state{}} |
  {noreply, NewState :: #bbs_port_controler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #bbs_port_controler_state{}} |
  {stop, Reason :: term(), NewState :: #bbs_port_controler_state{}}).


handle_call({create_win_named_pipe, PipeName}, _From, _State = #bbs_port_controler_state{}) ->
  case filelib:is_regular(PipeName) of
    true -> ok;
    false ->
      os:cmd("mkfifo " ++ PipeName)
  end;

handle_call({open_pipe_port, PipeName}, _From, State = #bbs_port_controler_state{}) ->
  PortPid = open_port(PipeName, [eof]),
  {reply, PortPid, State#bbs_port_controler_state{pipe_name = PipeName, port_pid = PortPid}};

handle_call(_Request, _From, State = #bbs_port_controler_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #bbs_port_controler_state{}) ->
  {noreply, NewState :: #bbs_port_controler_state{}} |
  {noreply, NewState :: #bbs_port_controler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #bbs_port_controler_state{}}).
handle_cast(_Request, State = #bbs_port_controler_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #bbs_port_controler_state{}) ->
  {noreply, NewState :: #bbs_port_controler_state{}} |
  {noreply, NewState :: #bbs_port_controler_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #bbs_port_controler_state{}}).
handle_info(_Info, State = #bbs_port_controler_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #bbs_port_controler_state{}) -> term()).
terminate(_Reason, _State = #bbs_port_controler_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #bbs_port_controler_state{},
    Extra :: term()) ->
  {ok, NewState :: #bbs_port_controler_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #bbs_port_controler_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
