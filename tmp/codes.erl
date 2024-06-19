

-module(codes).

-compile(export_all).

% 动态更新，这里如果用test:test()动态载入，如果你修改了test.erl，重新编译
% 输出的结果是新的

% 该热更新修改代码之后，旧的进程不会改变，新的进程会使用新的代码
% 旧版本开了一个进程不会变
% 会同时存在新旧版本
% 只会同时存在2个版本，如果出现新会直接把旧进程杀死

start()->
    put(pid,231),
    spawn(codes,loop,[]),
    tt().

tt()->
    Val= get(pid),
    io:format("pid:~p\n",[Val]).

loop()->
    sleep(2000),
    Val=test:test(),
    io:format("test:test()~p\n",[Val]),
    io:format("try ~p\n",[get(pid)]),
    loop().


sleep(Time)->
    receive
        after Time ->ok
    end.


% 进程字典
% put(键,值) 添加
% get(键) 获取
% get(). 获取所有
% erase(键) 删除
% 但在同一个进程作用域内有效