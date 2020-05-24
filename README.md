tc
=====

测试函数运行时间，

Example
-----
```
%% 执行N次函数调用，并统时长
tc:t(Module, Fun, Args, N).

%% 启动N个进程，分别执行执行一次函数调用，测试并发性能，并统计时长
tc:ct(Module, Fun, Args, N).
```

Build
-----

    $ rebar3 compile
