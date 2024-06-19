-module(hello).
-export([start/0,hello/1,numtest/0,stringtest/0,atomtest/0]).

start() ->
    io:fwrite("Hello, World!\n").

hello(X)->
    X.


% 基本数据类型
% 整数 浮点 没有上限

numtest()->
    9999999999999999999999999999999999999999999999*99999999999999999999999999999999999999999999999999999999999999.

% / 除 div 除 rem 取余

% numtest2()->
%     5 div 2.

% 字符串 ASCII 0-126 双引号包装
stringtest()->
    "hello".

% 原子 描述符号 以小写字母开头 常量 单引号强制为原子
atomtest()->
    'atom'.

%原组 以大括号包装
% {area,a,b}
%like: struct{
%    int a;
%   int b;
% }area;

% 列表 以中括号包装
% [{area,100,200,"?",{}},"sa",123,X]

% 变量 以大写字母开头
%X = 1.  % 不能重新赋值 除非f(X)函数解绑