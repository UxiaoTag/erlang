% 定义当前模块名为 `egame`
-module(egame).

% 定义模块的导出函数列表，这里只导出了一个函数 `start/0`
% 这意味着其他模块可以调用这个函数
-export([start/0]).

% 定义 `start/0` 函数
% 这个函数用于启动 `egame` 应用程序所需的所有依赖服务和应用程序
start() ->
    % 报错没有初始化poolboy，这里补上
    ok= application:start(poolboy),

    % 启动加密应用程序，用于提供加密功能
    ok = application:start(crypto),
    % 启动ASN.1编译器应用程序，用于处理ASN.1数据格式
    ok = application:start(asn1),
    % 启动公共密钥基础设施应用程序，用于处理公钥和证书
    ok = application:start(public_key),
    % 启动SSL/TLS应用程序，用于提供安全的网络连接
    ok = application:start(ssl),
    % 启动INETS应用程序，提供了一些基本的网络服务
    ok = application:start(inets),
    % 启动Xmerl应用程序，用于解析XML文档
    ok = application:start(xmerl),
    
    % 下面是启动Cowboy相关的应用程序，Cowboy是一个小型且高效的HTTP服务器框架
    % 启动Ranch，一个用于非阻塞TCP连接的Erlang库
    ok = application:start(ranch),
    % 启动Cowlib，Cowboy的底层库，提供了HTTP相关的基础功能
    ok = application:start(cowlib),
    % 启动Cowboy本身
    ok = application:start(cowboy),
    
    % 启动Jiffy，一个用于解析和生成JSON的Erlang库
    ok = application:start(jiffy),
    % 启动bcrypt，一个密码哈希库，用于安全地存储密码
    ok = application:start(bcrypt),

    % 启动eredis，Erlang的Redis客户端
    ok = application:start(eredis),
    % 启动epgsql，Erlang的PostgreSQL数据库客户端
    ok = application:start(epgsql),
    % 启动ailib，一个通用工具库，用于执行一些常见任务
    ok = application:start(ailib),
    % 启动aiconf，一个配置工具，用于生产环境的配置管理
    ok = application:start(aiconf),
    % 启动aidb，可能是用来与数据库进行交互的库
    ok = application:start(aidb),
    % 启动aihtml，一个HTML渲染库，可能是用于模板渲染
    ok = application:start(aihtml),
    
    % 最后启动egame应用程序本身
    ok = application:start(egame).