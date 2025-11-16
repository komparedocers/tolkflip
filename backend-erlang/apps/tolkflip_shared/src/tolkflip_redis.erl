%%%-------------------------------------------------------------------
%%% @doc High-performance Redis client with connection pooling
%%% @end
%%%-------------------------------------------------------------------
-module(tolkflip_redis).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    get/1, set/2, setex/3, del/1, exists/1,
    hget/2, hset/3, hdel/2, hgetall/1,
    incr/1, decr/1,
    lpush/2, lpop/1, lrange/3,
    publish/2
]).

-record(state, {conn}).

%%====================================================================
%% API
%%====================================================================

get(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["GET", Key]})
    end).

set(Key, Value) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["SET", Key, Value]})
    end).

setex(Key, Seconds, Value) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["SETEX", Key, integer_to_list(Seconds), Value]})
    end).

del(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["DEL", Key]})
    end).

exists(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["EXISTS", Key]})
    end).

hget(Key, Field) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["HGET", Key, Field]})
    end).

hset(Key, Field, Value) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["HSET", Key, Field, Value]})
    end).

hdel(Key, Field) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["HDEL", Key, Field]})
    end).

hgetall(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["HGETALL", Key]})
    end).

incr(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["INCR", Key]})
    end).

decr(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["DECR", Key]})
    end).

lpush(Key, Value) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["LPUSH", Key, Value]})
    end).

lpop(Key) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["LPOP", Key]})
    end).

lrange(Key, Start, Stop) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["LRANGE", Key, integer_to_list(Start), integer_to_list(Stop)]})
    end).

publish(Channel, Message) ->
    poolboy:transaction(redis_pool, fun(Worker) ->
        gen_server:call(Worker, {cmd, ["PUBLISH", Channel, Message]})
    end).

%%====================================================================
%% Poolboy Worker Callbacks
%%====================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),

    {ok, Conn} = eredis:start_link(Host, Port),
    {ok, #state{conn = Conn}}.

handle_call({cmd, Command}, _From, #state{conn = Conn} = State) ->
    Reply = eredis:q(Conn, Command),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    eredis:stop(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
