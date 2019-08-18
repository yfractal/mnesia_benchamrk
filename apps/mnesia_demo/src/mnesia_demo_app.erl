%%%-------------------------------------------------------------------
%% @doc mnesia_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
