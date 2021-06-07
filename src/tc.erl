-module(tc).

-export([t/2, t/3, t/4, ct/2, ct/3, ct/4]).

%% @doc 当前进程执行N次Fun()，打印结果
-spec t(Fun :: function(), N :: non_neg_integer()) -> ok.
t(Fun, N) ->
    M = erlang:fun_info(Fun, module),
    F = erlang:fun_info(Fun, name),
    A = erlang:fun_info(Fun, arity),
    do_t(Fun, M, F, A, N).

%% @doc 当前进程执行N次apply(Fun, A)，打印结果
-spec t(Fun :: function(), A :: [term()], N :: non_neg_integer()) -> ok.
t(Fun, A, N) ->
    Fun = fun() -> apply(Fun, A) end,
    M = erlang:fun_info(Fun, module),
    F = erlang:fun_info(Fun, name),
    do_t(Fun, M, F, A, N).

%% @doc 当前进程执行N次apply(M, F, A)，打印结果
-spec t(M :: module(), F :: atom(), A :: [term()], N :: non_neg_integer()) -> ok.
t(M, F, A, N) ->
    Fun = fun() -> apply(M, F, A) end,
    do_t(Fun, M, F, A, N).

%% @doc 启动N个进程执行Fun()，打印结果
-spec ct(Fun :: function(), N :: non_neg_integer()) -> ok.
ct(Fun, N) ->
    M = erlang:fun_info(Fun, module),
    F = erlang:fun_info(Fun, name),
    A = erlang:fun_info(Fun, arity),
    do_ct(Fun, M, F, A, N).

%% @doc 启动N个进程执行apply(Fun, A)，打印结果
-spec ct(Fun :: function(), A :: [term()], N :: non_neg_integer()) -> ok.
ct(Fun, A, N) ->
    Fun = fun() -> apply(Fun, A) end,
    M = erlang:fun_info(Fun, module),
    F = erlang:fun_info(Fun, name),
    do_ct(Fun, M, F, A, N).

%% @doc 启动N个进程执行apply(M, F, A)，打印结果
-spec ct(M :: module(), F :: atom(), A :: [term()], N :: non_neg_integer()) -> ok.
ct(M, F, A, N) ->
    Fun = fun() -> apply(M, F, A) end,
    do_ct(Fun, M, F, A, N).

%%----------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------

tc(Fun) ->
    {Microsecond, _} = timer:tc(Fun),
    Microsecond.

do_t(Fun, M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = loop(Fun, N),
    io:format("=====================~n"),
    io:format("execute [~p] times of {~p, ~p, ~P}:~n", [N, M, F, A, 9]),
    io:format("Maximum: ~p(μs)\t~p(s)~n", [Max, Max / 1000000]),
    io:format("Minimum: ~p(μs)\t~p(s)~n", [Min, Min / 1000000]),
    io:format("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format("Average: ~p(μs)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format("Greater: ~p~nLess: ~p~n", [Greater, Less]),
    io:format("=====================~n").

loop(Fun, N) ->
    loop(Fun, N, 1, 0, 0, 0, []).

loop(Fun, N, I, Max, Min, Sum, List) when N >= I ->
    Microsecond = tc(Fun),
    NewSum = Sum + Microsecond,
    if Max == 0 ->
        NewMax = NewMin = Microsecond;
        Max < Microsecond ->
            NewMax = Microsecond,
            NewMin = Min;
        Min > Microsecond ->
            NewMax = Max,
            NewMin = Microsecond;
        true ->
            NewMax = Max,
            NewMin = Min
    end,
    loop({M, F, A}, N, I + 1, NewMax, NewMin, NewSum,
        [Microsecond | List]);
loop({_M, _F, _A}, N, _I, Max, Min, Sum, List) ->
    Aver = Sum / N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

distribution(List, Aver) ->
    distribution(List, Aver, 0, 0).

distribution([H | T], Aver, Greater, Less) ->
    case H > Aver of
        true ->
            distribution(T, Aver, Greater + 1, Less);
        false ->
            distribution(T, Aver, Greater, Less + 1)
    end;
distribution([], _Aver, Greater, Less) ->
    {Greater, Less}.

do_ct(Fun, M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = cloop(Fun, N),
    io:format("=====================~n"),
    io:format("spawn [~p] processes of {~p, ~p, ~P}:~n", [N, M, F, A, 9]),
    io:format("Maximum: ~p(μs)\t~p(s)~n", [Max, Max / 1000000]),
    io:format("Minimum: ~p(μs)\t~p(s)~n", [Min, Min / 1000000]),
    io:format("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format("Average: ~p(μs)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format("Greater: ~p~nLess: ~p~n", [Greater, Less]),
    io:format("=====================~n").

cloop(Fun, N) ->
    CollectorPid = self(),
    ok = loop_spawn(Fun, CollectorPid, N),
    collector(0, 0, 0, N, 1, []).

loop_spawn(Fun, CollectorPid, N) when N > 0 ->
    spawn_link(fun() -> worker(Fun, CollectorPid) end),
    loop_spawn(Fun, CollectorPid, N - 1);
loop_spawn(_, _, 0) ->
    ok.

collector(Max, Min, Sum, N, I, List) when N >= I ->
    receive
        {result, Microsecond} ->
            NewSum = Sum + Microsecond,
            if Max == 0 ->
                NewMax = NewMin = Microsecond;
                Max < Microsecond ->
                    NewMax = Microsecond,
                    NewMin = Min;
                Min > Microsecond ->
                    NewMax = Max,
                    NewMin = Microsecond;
                true ->
                    NewMax = Max,
                    NewMin = Min
            end,
            collector(NewMax, NewMin, NewSum, N, I + 1, [Microsecond | List])
    after 100000 ->
        ok
    end;
collector(Max, Min, Sum, N, _, List) ->
    Aver = Sum / N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

worker(Fun, CollectorPid) ->
    Microsecond = tc(Fun),
    CollectorPid ! {result, Microsecond}.

