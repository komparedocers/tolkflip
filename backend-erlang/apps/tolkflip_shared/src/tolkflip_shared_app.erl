%%%-------------------------------------------------------------------
%%% @doc Tolkflip Shared Application
%%% @end
%%%-------------------------------------------------------------------
-module(tolkflip_shared_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting Tolkflip Shared Application"),

    %% Start Cassandra connection
    ok = start_cassandra(),

    %% Start Redis pool
    ok = start_redis_pool(),

    tolkflip_shared_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_cassandra() ->
    {ok, CassandraConfig} = application:get_env(tolkflip_shared, cassandra),
    ContactPoints = proplists:get_value(contact_points, CassandraConfig),
    Port = proplists:get_value(port, CassandraConfig),
    Keyspace = proplists:get_value(keyspace, CassandraConfig),

    %% Initialize Cassandra cluster
    ClusterOpts = [
        {contact_points, ContactPoints},
        {port, Port},
        {keyspace, Keyspace},
        {number_threads_io, 4},
        {queue_size_io, 8192},
        {core_connections_per_host, 2},
        {max_connections_per_host, 8}
    ],

    case erlcass:create_session(ClusterOpts) of
        {ok, _SessionRef} ->
            lager:info("Cassandra session created successfully"),
            ok;
        {error, Reason} ->
            lager:error("Failed to create Cassandra session: ~p", [Reason]),
            {error, Reason}
    end.

start_redis_pool() ->
    {ok, RedisConfig} = application:get_env(tolkflip_shared, redis),
    Host = proplists:get_value(host, RedisConfig),
    Port = proplists:get_value(port, RedisConfig),
    PoolSize = proplists:get_value(pool_size, RedisConfig),

    PoolArgs = [
        {name, {local, redis_pool}},
        {worker_module, tolkflip_redis},
        {size, PoolSize},
        {max_overflow, PoolSize * 2}
    ],

    WorkerArgs = [
        {host, Host},
        {port, Port}
    ],

    PoolSpec = poolboy:child_spec(redis_pool, PoolArgs, WorkerArgs),

    case supervisor:start_child(tolkflip_shared_sup, PoolSpec) of
        {ok, _Pid} ->
            lager:info("Redis pool started with ~p workers", [PoolSize]),
            ok;
        {error, Reason} ->
            lager:error("Failed to start Redis pool: ~p", [Reason]),
            {error, Reason}
    end.
