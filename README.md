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

```
