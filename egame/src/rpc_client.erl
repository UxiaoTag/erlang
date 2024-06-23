-module(rpc_client).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
-export([call/3]).
-define(SERVER, ?MODULE).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {socket,buffer,caller}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(integer()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
call(Moudle,Function,Params)->
  gen_server:call(?SERVER,{Moudle,Function,Params}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Port]) ->
    SomeHostInNet = "127.0.0.1", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port, 
                                 [binary, {packet, 0}]),
    % 设置套接字为活动模式，但仅在下一次数据到达时发送通知。
    % 这允许我们控制何时接收数据，减少不必要的消息处理，并避免潜在的消息积压。
    inet:setopts(Sock, [{active, once}]),
    {ok, #state{socket = Sock,buffer = rpc_protocol:new()}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call({Moudle,Function,Params}, From,
            #state{socket = Socket}=State) ->
    Binary=rpc_protocol:encode(Moudle,Function,Params),
    ok=gen_tcp:send(Socket,Binary),
    % ok之后还要判断
    {noreply, State#state{caller = From}}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    State0=handle_data(Data,State),
    % 处理从服务器接收到的数据
    io:format("Received: ~p~n", [Data]),
    % 设置套接字为活动模式，但仅在下一次数据到达时发送通知。
    inet:setopts(Socket, [{active, once}]),
    {noreply, State0};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Socket})-> 
    gen_tcp:close(Socket),
    ok.



%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
handle_data(Data,#state{buffer = Buffer,caller = Caller}=State)->
    {Reps,Buffer0}=rpc_protocol:decode(Data,Buffer),
    case Reps of
        [Rep]->
            gen_server:reply(Caller,Rep),
            State#state{buffer = Buffer0,caller = undefined}; 
        []->
            State#state{buffer = Buffer0}
        end.