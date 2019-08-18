-module(benchmark).

-export([create_users/1,
         create_users/2,
         benchmark_create_users/1,
         create_users_in_process/2]).

create_users(N) ->
    create_users(1, N).

create_users(Start, N) ->
    Rv = lists:map(fun(I) -> mnesia_demo:write({I, I}) end,
              lists:seq(Start, (Start + N))),
    Rv.

create_users_in_process(UsersCount, ProcessesCount) ->
    MonitorPid = self(),
    WorkerPids =
        lists:map(
          fun(I) ->
                  N = round(UsersCount / ProcessesCount),
                  Start = N * ( I - 1 ),
                  spawn(fun() -> create_users_worker(MonitorPid, Start, N) end)
          end,
          lists:seq(1, ProcessesCount)),

    T0 = get_timestamp(),

    lists:map(fun(Pid) -> Pid ! start end,
              WorkerPids),
    waiter(ProcessesCount, finised),
    T1 = get_timestamp(),
    print_time_takes(create_users_in_process, T0, T1).

waiter(N, Mesage) ->
    waiter(0, N, Mesage).
waiter(Received, N, Mesage) ->
    receive
        Mesage ->
            case Received + 1 == N of
                true -> ok;
                false -> waiter(Received + 1, N, Mesage)
            end
    end.

create_users_worker(MonitorPid, Start, N) ->
    receive
        start ->
            %% io:format("start worker: MonitorPid=~p ~n", [MonitorPid]),
            create_users(Start, N),
            MonitorPid ! finised
    end.

benchmark_create_users(N) ->
    T0 = get_timestamp(),
    create_users(N),
    T1 = get_timestamp(),
    print_time_takes(create_users, T0, T1).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro/1000).

print_time_takes(Name, T0, T1) ->
    Takes = T1 - T0,
    io:format("~p takes ~p milliseconds", [Name, Takes]).
