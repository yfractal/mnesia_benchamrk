mnesia_demo
=====

An OTP application

Build
-----

    $ rebar3 compile

Dev
----

``` erlang

{ok, Pid} = mnesia_demo:start_link().

mnesia_demo:write(Pid, {1, "Mike"}).

mnesia_demo:find(Pid, 1).

%% benchmark:create_users().

benchmark:benchmark_create_users(10000).

benchmark:create_users_in_process(10000, 4).

```
