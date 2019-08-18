-module(mnesia_demo).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create_table/0,
         write/2,
         find/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(user_record, {id, name}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

create_table() ->
    do_create_table().

write(Pid, {Id, Name}) ->
    gen_server:call(Pid, {write, Id, Name}).

find(Pid, Id) ->
    gen_server:call(Pid, {find, Id}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({write, Id, Name}, _From, State) ->
    Reply = ok,
    do_write(Id, Name),
    {reply, Reply, State};
handle_call({find, Id}, _From, State) ->
    Reply = do_find(Id),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_create_table() ->
    RV = mnesia:create_table(user_record,
                             [{ram_copies, [node() | nodes()]},
                              {attributes, record_info(fields, user_record)}]),
    case RV of
        {atomic, ok} ->
            {ok};
        {aborted, Reason} ->
            {error, Reason}
    end.

do_write(Id, Name) ->
    User = #user_record{id=Id, name=Name},
    F = fun() -> mnesia:write(User) end,
    mnesia:transaction(F).

do_find(Id) ->
    F = fun() -> mnesia:read({user_record, Id}) end,
    mnesia:transaction(F).
