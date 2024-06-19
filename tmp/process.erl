-module(process).

-compile(export_all).


% 进程很轻量 数据类型
% 进程 pid
% spwan(model,func,[]).
% receive 模式匹配
% Pid ! Msg 发送消息

go()->
    Server=spawn(process,loop,[]),
    Server ! {self(),hello},
    receive
        {Pid,Msg}->
            io:format("~p receive msg:~p\n", [Pid, Msg])
    end,
    Server ! {self(),stop},
    true.

loop() ->
    receive
        {Pid, Msg} ->
            case Msg of
                stop ->
                    io:format("stop\n");
                _ ->
                    io:format("~p receive msg:~p\n", [Pid, Msg]),
                    Pid ! {self(), Msg},
                    loop()
            end
    end.

% 进程注册
% 因为spawn是创建新的进程，但是实际使用会用一些公用进程，所以需要注册
% 使用别名来标识进程

% register(name,Pid) 注册进程
% whereis(name) 获取进程
% unregister(name) 注销进程

go1()->
    % 主进程注册goprocess
    register(goprocess,self()),
    % loop进程注册lServer
    register(lServer,spawn(process,loop1,[])),
    sleep(5000),
    
    lServer ! {goprocess,hello},
    receive
        {Pid,Msg}->
            io:format("~p receive msg:~p\n", [Pid, Msg])
    end,
    lServer ! {goprocess,stop},
    true.

loop1() ->
    receive
        {Pid, Msg} ->
            case Msg of
                stop ->
                    io:format("stop\n");
                _ ->
                    io:format("~p receive msg:~p\n", [Pid, Msg]),
                    goprocess ! {lServer, Msg},
                    loop1()
            end
    after
        1000->
            io:format("no msg\n"),
            loop1()
    end.

sleep(Time)->
    receive
    after Time->
        ok
    end.