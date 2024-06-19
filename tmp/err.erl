-module(err).
-compile(export_all).

% errro 运行时错误
% exit exit退出
% throw 主动引发错误
% after try执行完毕执行
% of try里面不同的分支
test()->
    try
        % 1/0
        % throw("error")
        exit("exit")
    catch
        error:Error:Stack ->{Error,Stack};
        exit:Exit ->{exit,Exit};
        throw:Throw ->{throw,Throw}
    after
        io:fwrite("end\n")
    end.

% 一般try执行完毕之后
% 执行after
% of进行分支比对
% 一般of用于正确的情况 catch用于错误的情况

test2()->
    try
        1-1
    of
        0->ok;
        10->error
    after
        io:fwrite("likai try\n")
    end.