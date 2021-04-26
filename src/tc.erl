-module(tc).

-lager_records([]).

-export([t/4, ct/4]).

%% @doc 当前进程执行N次MFA，打印结果
-spec t(module(), function(), [term()], non_neg_integer()) -> ok.
t(M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = loop({M, F, A},
						N),
    io:format("=====================~n"),
    io:format("execute [~p] times of {~p, ~p, ~P}:~n",
	      [N, M, F, A, 9]),P
    io:format("Maximum: ~p(μs)\t~p(s)~n",
	      [Max, Max / 1000000]),
    io:format("Minimum: ~p(μs)\t~p(s)~n",
	      [Min, Min / 1000000]),
    io:format("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format("Average: ~p(μs)\t~p(s)~n",
	      [Aver, Aver / 1000000]),
    io:format("Greater: ~p~nLess: ~p~n", [Greater, Less]),
    io:format("=====================~n").

%% @doc 启动N个进程执行MFA，打印结果
-spec ct(module(), function(), [term()], non_neg_integer()) -> ok.
ct(M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = cloop({M, F, A},
						 N),
    io:format("=====================~n"),
    io:format("spawn [~p] processes of {~p, ~p, ~P}:~n",
	      [N, M, F, A, 9]),
    io:format("Maximum: ~p(μs)\t~p(s)~n",
	      [Max, Max / 1000000]),
    io:format("Minimum: ~p(μs)\t~p(s)~n",
	      [Min, Min / 1000000]),
    io:format("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format("Average: ~p(μs)\t~p(s)~n",
	      [Aver, Aver / 1000000]),
    io:format("Greater: ~p~nLess: ~p~n", [Greater, Less]),
    io:format("=====================~n").

%%----------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------
loop({M, F, A}, N) ->
    loop({M, F, A}, N, 1, 0, 0, 0, []).

loop({M, F, A}, N, I, Max, Min, Sum, List)
    when N >= I ->
    Microsecond = tc(M, F, A),
    NewSum = Sum + Microsecond,
    if Max == 0 -> NewMax = NewMin = Microsecond;
       Max < Microsecond -> NewMax = Microsecond, NewMin = Min;
       Min > Microsecond -> NewMax = Max, NewMin = Microsecond;
       true -> NewMax = Max, NewMin = Min
    end,
    loop({M, F, A}, N, I + 1, NewMax, NewMin, NewSum,
	 [Microsecond | List]);
loop({_M, _F, _A}, N, _I, Max, Min, Sum, List) ->
    Aver = Sum / N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

tc(M, F, A) ->
    {Microsecond, _} = timer:tc(M, F, A), Microsecond.


distribution(List, Aver) ->
    distribution(List, Aver, 0, 0).

distribution([H | T], Aver, Greater, Less) ->
    case H > Aver of
      true -> distribution(T, Aver, Greater + 1, Less);
      false -> distribution(T, Aver, Greater, Less + 1)
    end;
distribution([], _Aver, Greater, Less) ->
    {Greater, Less}.

cloop({M, F, A}, N) ->
    CollectorPid = self(),
    ok = loop_spawn({M, F, A}, CollectorPid, N),
    collector(0, 0, 0, N, 1, []).

loop_spawn({M, F, A}, CollectorPid, N) when N > 0 ->
    spawn_link(fun () -> worker({M, F, A}, CollectorPid)
	       end),
    loop_spawn({M, F, A}, CollectorPid, N - 1);
loop_spawn(_, _, 0) -> ok.

collector(Max, Min, Sum, N, I, List) when N >= I ->
    receive
      {result, Microsecond} ->
	  NewSum = Sum + Microsecond,
	  if Max == 0 -> NewMax = NewMin = Microsecond;
	     Max < Microsecond -> NewMax = Microsecond, NewMin = Min;
	     Min > Microsecond -> NewMax = Max, NewMin = Microsecond;
	     true -> NewMax = Max, NewMin = Min
	  end,
	  collector(NewMax, NewMin, NewSum, N, I + 1,
		    [Microsecond | List])
      after 100000 -> ok
    end;
collector(Max, Min, Sum, N, _, List) ->
    Aver = Sum / N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

worker({M, F, A}, CollectorPid) ->
    Microsecond = tc(M, F, A),
    CollectorPid ! {result, Microsecond}.

