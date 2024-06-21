# 关于erlang

1991年的语言(还挺年轻)，函数式编程语言，甚至没有循环，变量也只能用一次。

但是适合多线程(在go没出来之前，多线程服务器好像还蛮爱用erlang的，主要他的多进程很好玩，是用了一个信箱制度)

```erlang
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
```

蛮有意思的玩意，虽然我感觉多进程其实没用goroutines好用就是了。


## 一些历史

OTP18之前没有map，实现map就通过proplists实现（特殊的列表，其中每个表的kv，k唯一，但是有性能问题）

R15B的时候主要支持单核cpu为主,vsmp（没了解过）

R15B之后就慢慢支持SMP了，到现在（了解SMP架构）

erlang从OTP22版本以及以后，就开始支持IPSA的模型，在之前就只有其他model(没查明白)，在smp中IO就很有不错的提升。




## 关于项目
erlang有erlang.mk，erbar3。目前都是后者居多

俩种make erlang项目的玩意创建erlang会有多种形态

application的话默认会创建一个app，里面的start，stop方法会创建一个进行sup，当然sup里面会有init()

libraries则是文件的聚合体，并不会产生进程，以及监控数

release主要负责对多个applicantion进行打包，打包成可用的包


## 执行完毕之后在主src文件中添加相关模块的applictaion:start()


然后可以跑这条命令看一些图形化的东西，我现在也看不懂observer:start().

开发环境配置基本完成