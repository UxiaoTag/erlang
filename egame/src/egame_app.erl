-module(egame_app).
% 类似interface{start() stop()}
% 这个start其实是创建一个demosup进程
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	egame_sup:start_link().

stop(_State) ->
	ok.
