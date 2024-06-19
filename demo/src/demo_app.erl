%%%-------------------------------------------------------------------
%% @doc demo public API
%% @end
%%%-------------------------------------------------------------------

-module(demo_app).

-behaviour(application).

-export([start/2, stop/1,start/0]).


start()->
    io:format("hello World~n"),
    StartType=startType,
    StartArgs=startArgs,
    start(StartType,StartArgs).

start(_StartType, _StartArgs) ->
    demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
