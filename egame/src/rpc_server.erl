-module(rpc_server).

-export([start_server/1,
         start_link/4,
         init/2]).

% 定义服务器的状态记录，包含传输方式、套接字和缓冲区。
-record(state, {transport,
                socket,
                buffer}).

% 启动RPC服务器监听指定的端口。
start_server(Port)->
    ranch:start_listener(?MODULE, ranch_tcp, [{port, Port}], ?MODULE, undefined).

% 为每个连接创建一个新的进程。
start_link(Ref, _Socket, Transport, _Opts)->
    Pid = spawn_link(?MODULE, init, [Ref, Transport]),
    {ok, Pid}.

% 初始化新连接的进程。
init(Ref, Transport)->
    {ok, Socket} = ranch:handshake(Ref), % 完成ranch的握手过程。
    Buffer = rpc_protocol:new(), % 初始化一个新的缓冲区。
    loop(#state{transport=Transport, socket=Socket, buffer=Buffer}). % 进入循环处理消息。

% 主循环，处理接收到的消息和事件。
loop(ok)->ok;
loop(#state{transport = Transport, socket = Socket}=State)->
    % 获取传输层的消息状态。
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [{active, once}]), % 设置套接字为主动一次模式。
    Result =
        receive
            % 接收到数据。
            {OK, Socket, Data}->
                {Resp, State0} = handle_data(Data, State), % 处理接收到的数据。
                reply(Resp, State0); % 回复请求。
            % 连接关闭。
            {Closed, Socket}-> ok;
            % 出现错误。
            {Error, Socket, _Error}-> ok
        end,
    % 继续循环。
    loop(Result).

% 处理接收到的数据。
handle_data(Data, #state{buffer = Buffer}=State)->
    % 使用rpc_protocol:decode解码数据。
    {Reqs, Buffer0} = rpc_protocol:decode(Buffer, Data),
    Reps = handle_request(Reqs), % 处理请求。
    {Reps, State#state{buffer=Buffer0}}. % 更新状态并返回。

% 处理请求列表。
handle_request([])-> undefined;
handle_request(Reqs)->
    % 递归处理请求列表。
    handle_request(Reqs, []).

% 辅助函数，用于反转累积的响应列表。
handle_request([], Acc)->
    lists:reverse(Acc);
handle_request([{Module, Function, Params}|T], Acc)->
    % 应用函数并处理请求。
    Rep = erlang:apply(Module, Function, Params),
    % 继续处理剩余的请求。
    handle_request(T, [Rep|Acc]).

% 回复请求。
reply([], State)->
    State;
reply([Rep|T], #state{transport = Transport, socket = Socket}=State)->
    % 编码响应数据。
    Frame = rpc_protocol:encode(Rep),
    % 发送响应数据。
    case Transport:send(Socket,Frame) of
        ok-> reply(T, State); % 如果发送成功，继续发送剩余的响应。
        _ -> ok % 发送失败，这里简单返回ok，实际应用中可能需要更复杂的错误处理。
    end.