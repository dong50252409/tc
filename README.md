tc
=====

测试函数运行时间，

Example
-----
```
%% 执行apply(Module,F,Args)10次，并统时长
tc:t(Module, Fun, Args, N).

%% 启动10个进程，分别执行apply(Module,F,Args)，测试并发性能，并统计时长
tc:ct(Module, Fun, Args, 10).
```

Build
-----

    $ rebar3 compile
