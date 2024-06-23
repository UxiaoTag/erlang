-module(rpc_protocol).

-export([encode/3, encode/1, decode/2]).
-export([new/0]).

-record(rpc_protocol, {buffer}).

% 初始化一个新的 rpc_protocol 记录，其 buffer 字段为空二进制。
new() ->
    #rpc_protocol{buffer = <<>>}.

% 编码 RPC 调用请求。
% 参数:
%   Module - 原子，表示要调用的模块名。
%   Function - 原子，表示要调用的函数名。
%   Params - 整数列表，表示调用参数。
% 返回值:
%   返回一个二进制格式的数据，包含了模块名、函数名、参数大小和参数列表。
-spec encode(atom(), atom(), list()) -> binary().
encode(Module, Function, Params) ->
    Module0 = erlang:atom_to_binary(Module, utf8),  % 将模块名转换为 UTF-8 编码的二进制。
    Function0 = erlang:atom_to_binary(Function, utf8),  % 将函数名转换为 UTF-8 编码的二进制。
    ParamSize = erlang:length(Params),  % 计算参数列表的长度。
    ModuleLen = erlang:byte_size(Module0),  % 获取模块名二进制数据的大小。
    FunctionLen = erlang:byte_size(Function0),  % 获取函数名二进制数据的大小。
    E = <<ModuleLen:16/big-integer, Module0/binary,  % 构造模块名长度和模块名。
         FunctionLen:16/big-integer, Function0/binary,  % 构造函数名长度和函数名。
         ParamSize:32/big-integer>>,  % 构造参数大小。
    E0 = lists:foldl(fun(Param, Acc) ->  % 将参数列表转换为二进制并追加。
                       <<Acc/binary, Param/big-integer>>
                   end, E, Params),
    ELen = erlang:byte_size(E0) + 1,  % 计算最终数据的长度。
    <<ELen:32/big-integer, 0:8/big-integer, E0/binary>>.  % 构造最终的二进制数据，包含长度和数据。

% 编码 RPC 调用结果。
% 参数:
%   Result - 整数，表示调用结果。
% 返回值:
%   返回一个二进制格式的数据，包含了结果数据。
-spec encode(integer()) -> binary().
encode(Result) ->
    Result0 = erlang:integer_to_binary(Result),  % 将整数结果转换为二进制。
    ELen = erlang:byte_size(Result0) + 1,  % 计算结果数据的长度。
    <<ELen:32/big-integer, 1:8/big-integer, Result0/binary>>.  % 构造包含类型和结果的二进制数据。

% 解码 RPC 请求或结果。
% 参数:
%   Packet - 接收到的二进制数据包。
%   Buffer - 当前缓冲区，用于处理分包数据。
% 返回值:
%   返回解码后的请求列表和剩余的缓冲区数据。
-spec decode(binary(), term()) -> {list(), term()}.
decode(Packet, #rpc_protocol{buffer = Buffer}) ->
    Buffer0 = <<Buffer/binary, Packet/binary>>,  % 将新数据包追加到当前缓冲区。
    {Requests, Rem} = decode(Buffer0),  % 解码合并后的缓冲区。
    {Requests, #rpc_protocol{buffer = Rem}}.  % 返回请求列表和更新后的缓冲区。

% 辅助函数，用于递归解码缓冲区中的数据。
decode(Buffer) ->
    decode_buffer(Buffer, []).

% 解码辅助函数，处理不同类型的数据包。
decode_buffer(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};  % 如果缓冲区为空，返回累积的请求列表和空缓冲区。
decode_buffer(<<Len:32/big-integer, Payload/binary>> = Buffer, Acc)
        when byte_size(Payload) < Len ->
    {lists:reverse(Acc), Buffer};  % 如果数据不足，返回累积的请求列表和当前缓冲区。
decode_buffer(<<Len:32/big-integer, Type:8/big-integer, Payload/binary>>, Acc) ->
    decode_buffer(Len, Type, Payload, Acc).  % 根据长度和类型进一步解码。

% 解码函数调用请求。
decode_buffer(_, 0, Payload, Acc) ->
    <<ModuleLen:16/big-integer, Module:ModuleLen/binary,
      FunctionLen:16/big-integer, Function:FunctionLen/binary,
      ParamSize:32/big-integer, Rest/binary>> = Payload,
    {Params, Rest0} = lists:foldl(
        fun(_, {ParamsAcc, Binary}) ->
            <<Param/big-integer, Binary0/binary>> = Binary,
            {[Param | ParamsAcc], Binary0}
        end, {[], Rest}, lists:seq(0, ParamSize - 1)
    ),
    decode_buffer(Rest0, [{erlang:binary_to_atom(Module, utf8),
                             erlang:binary_to_atom(Function, utf8),
                             lists:reverse(Params)} | Acc]);
% 解码整数结果。
decode_buffer(Len, 1, Payload, Acc) ->
    Len0 = (Len - 1) * 8,  % 计算结果数据的实际长度（位）。
    <<Result:Len0/bits, Rest/bits>> = Payload,
    decode_buffer(Rest, [erlang:binary_to_integer(Result) | Acc]).