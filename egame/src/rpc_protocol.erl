-module(rpc_protocol).

-export([encode/3]).


% 状态记录定义，用于存储解码过程中的缓冲区数据。
-record(rpc_protocol, {buffer= <<>> }). % 初始为空二进制

% 支持int
% packet length: 4
% model name length: 2
% model name :variable
% function name length: 2
% function name: variable
% parameter size: 2
% parmeters: ...

-spec encode(atom(),atom(),list()) -> binary().

% Type 0 带有模块名、函数名和参数列表的请求
encode(Module, Function, Params) ->
    % 将模块名从原子转换为UTF-8编码的二进制格式。
    Module0 = erlang:atom_to_binary(Module, utf8),
    
    % 将函数名从原子转换为UTF-8编码的二进制格式。
    Function0 = erlang:atom_to_binary(Function, utf8),
    
    % 计算参数列表的长度。
    ParmSize = erlang:length(Params),
    
    % 获取模块名二进制数据的大小（字节数）。
    ModuleLen = erlang:byte_size(Module0),
    
    % 获取函数名二进制数据的大小（字节数）。
    FunctionLen = erlang:byte_size(Function0),
    
    % 构造初始的二进制数据，包含模块名长度、模块名、函数名长度和函数名。
    E = <<ModuleLen:16/big-integer, Module0/binary,
         FunctionLen:16/big-integer, Function0/binary,
         ParmSize:32/big-integer>>,
    
    % 遍历参数列表，将每个参数转换为大整数格式，并追加到E之后。
    E0 = list:foldl(fun(Param, Acc) ->
        <<Acc/binary, Param/big-integer>>
    end, E, Params),
    
    % 获取最终二进制数据的大小（字节数）。
    Elen = erlang:byte_size(E0)+1,
    
    % 构造最终的二进制数据，包含整个数据的长度和数据本身。
    % 这样接收方可以首先读取长度，然后根据长度读取后续的数据。
    % Len Type Payload
    <<Elen:32/big-integer,0:8/big-integer, E0/binary>>.

% Type 1 带有整数结果的请求
-spec encode(integer()) -> binary().
encode(Result)->
  Result0 = erlang:integer_to_binary(Result),
  ELen = erlang:byte_size(Result0) + 1,
  <<ELen:32/big-integer,1:8/big-integer,Result0/binary>>.

    % decode会粘包

-spec decode(binary(),term())->{list(),binary()}.
decode(Packet,#rpc_protocol{buffer=Buffer}) ->
    % 将新接收到的数据包与当前缓冲区合并。
    Buffer0 = <<Buffer/binary,Packet/binary>>,
    % 调用递归解码函数 decode_buffer/2 处理合并后的缓冲区。
    {Requset,Rem}=decode(Buffer0),
    % 返回解码得到的请求列表和更新后的缓冲区状态。
    {Requset,#rpc_protocol{buffer=Rem}}.

% 递归解码函数，尝试从Buffer中解码出完整的请求。
% Acc 是一个累加器，用于收集解码出的请求。
% 接收一个二进制数据 Buffer 并初始化累加器 Acc 为一个空列表。
decode(Buffer) ->
    decode_buffer(Buffer,[]).

% 解码函数的辅助函数，用于从 Buffer 中解码请求。
% 当 Buffer 为空时，返回累加器 Acc 中的请求列表和空的二进制数据。
decode_buffer(<<>>,Acc)->
    {lists:reverse(Acc),<<>>};
% 当 Buffer 中的数据不足以构成一个完整的请求时（即小于请求长度 Len），返回当前的累加器 Acc 和 Buffer。
decode_buffer(<<Len:32/big-integer,Payload/binary>> = Buffer,Acc)
    when erlang:byte_size(Buffer)<Len->
    {lists:reverse(Acc),Buffer};
% 当 Buffer 中的数据足够构成一个完整的请求时，解码请求并递归调用自身处理剩余的数据。
% 这里 Len 是请求数据的总长度，Type 是请求的类型。
decode_buffer(<<Len:32/big-integer,Type:8/big-integer,Payload/binary>>,Acc)->
    decode_buffer(Len,Type,Payload,Acc).

% 当 Type 为 0 时，表示请求包含模块名、函数名和参数列表。
% 这里我们首先解码模块名和函数名的长度和值，然后是参数的数量 ParamSize。
% 接着，我们使用 foldl 函数来解码参数列表，假设每个参数都是一个整数。
decode_buffer(_,0,Payload,Acc)->
    <<ModuleLen:16/big-integer,Module:ModuleLen/binary,
    FunctionLen:16/big-integer,Function:FunctionLen/binary,
    ParamSize:32/big-integer,Rest/binary>> = Payload,
  {Params,Rest0} = lists:foldl(
                     fun(_,{ParamsAcc,Binary})->
                         <<Param/big-integer,Binary0/binary>> = Binary,
                         {[Param|ParamsAcc],Binary0}
                     end,{[],Rest}, lists:seq(0,ParamSize - 1)),
  decode_buffer(Rest0,[{erlang:binary_to_atom(Module, utf8),
                        erlang:binary_to_atom(Function,utf8),
                        lists:reverse(Params)
                       }|Acc]);
% 当 Type 为 1 时，表示请求包含一个整数类型的结果。
% 这里我们计算 Result 的长度 Len0，并从 Payload 中解码出整数 Result。
% 注意 Len0 需要乘以 8 因为我们处理的是位（bits）而不是字节（bytes）。
decode_buffer(Len,1,Payload,Acc) ->
  Len0 = (Len - 1) * 8,
  <<Result:Len0/bits,Rest/bits>> = Payload,
    % 将解码的结果添加到累加器 Acc 中，并继续解码剩余的 Rest。
  decode_buffer(Rest,[erlang:binary_to_integer(Result)|Acc]).

