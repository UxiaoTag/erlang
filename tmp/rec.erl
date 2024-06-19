-module(rec).

-compile(export_all).


-include("task.hrl").
% 记录，进阶的元组 {name,X,Y}[] 字典
% 任务系统 任务标题 是否开始 优先级

add_task(Tile,Start,Infer)->
    {Tile,Start,Infer}.

get_infer({_,_,Infer})->Infer.

start_task({Tile,_,Infer})->
    {Tile,true,Infer}.





add_task2(Tile,Start,Infer)->
    #task{tile=Tile,start=Start,infer=Infer}.

get_infer2(Rec)->
    Rec#task.infer.

start_task2(Rec)->
    Rec#task{tile=Rec#task.tile,start=true}.
