

-module(test).

-compile(export_all).

test()->hello_world.

% 函数名必须是原子 Test()->hello.


% 简单的模式匹配，但是这样看不习惯
calc(add,X,Y)->X+Y;
calc(sub,X,Y)->X-Y;
calc(_,_,_)->error.


% 方法2但是方法用的比较多
calc({X,Y,Z})->
    case X of
        add->Y+Z;
        sub->Y-Z;
        _->error
    end.


cost(apple)->5;
cost(orange)->10;
cost(peach)->7.

% [{apple,5},{orange,10},{peach,7}]
% 没有循环递归
shop([])->0;
shop([{Name,Num}|T])->
    cost(Name)*Num+shop(T).

%匿名函数
functest(num)->
    fun(X)->X+1 end;
functest(_)->
    lists:map(fun(X)->X+1 end,[1,2,3,4,5]).

functest2()->
    lists:filter(fun(X)->X rem 2==0 end,[1,2,3,4,5]). % 过滤偶数

