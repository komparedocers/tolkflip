%%%-------------------------------------------------------------------
%%% @doc API Gateway HTTP Handler
%%% Proxies HTTP requests to backend services
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_handler).

-export([init/2]).

-define(SERVICE_PORTS, #{
    auth => 3001,
    user => 3002,
    chat => 3003,
    translation => 3004,
    transcription => 3005,
    media => 3006,
    presence => 3007,
    message_store => 3008,
    notification => 3009,
    webrtc => 3010,
    group => 3011
}).

init(Req0, State) ->
    Service = maps:get(service, State),
    Method = cowboy_req:method(Req0),

    %% Build target URL
    Port = maps:get(Service, ?SERVICE_PORTS),
    Path = build_path(Req0, State),
    TargetUrl = io_lib:format("http://localhost:~p~s", [Port, Path]),

    %% Proxy request
    case proxy_request(Method, TargetUrl, Req0) of
        {ok, StatusCode, Headers, Body} ->
            Req = cowboy_req:reply(StatusCode, maps:from_list(Headers), Body, Req0),
            {ok, Req, State};
        {error, Reason} ->
            lager:error("Gateway proxy error for ~s ~s: ~p", [Method, TargetUrl, Reason]),
            Req = cowboy_req:reply(502,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Bad Gateway">>}),
                Req0),
            {ok, Req, State}
    end.

build_path(Req, State) ->
    case maps:get(path, State, undefined) of
        undefined ->
            %% Use path_pattern with bindings
            PathPattern = maps:get(path_pattern, State),
            Bindings = cowboy_req:bindings(Req),
            replace_bindings(PathPattern, Bindings);
        Path ->
            %% Use fixed path
            QsVals = cowboy_req:qs(Req),
            case QsVals of
                <<>> -> binary_to_list(Path);
                _ -> binary_to_list(Path) ++ "?" ++ binary_to_list(QsVals)
            end
    end.

replace_bindings(PathPattern, Bindings) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        Pattern = <<":", Key/binary>>,
        binary:replace(Acc, Pattern, Value, [global])
    end, PathPattern, maps:to_list(Bindings)).

proxy_request(Method, Url, Req) ->
    %% Read request body if present
    {ok, Body, _Req2} = cowboy_req:read_body(Req),

    %% Get headers
    Headers = cowboy_req:headers(Req),
    HeaderList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- maps:to_list(Headers)],

    %% Make HTTP request (using httpc)
    Request = case Method of
        <<"GET">> ->
            {Url, HeaderList};
        <<"DELETE">> ->
            {Url, HeaderList};
        _ ->
            ContentType = case lists:keyfind("content-type", 1, HeaderList) of
                {_, CT} -> CT;
                false -> "application/json"
            end,
            {Url, HeaderList, ContentType, Body}
    end,

    MethodAtom = binary_to_existing_atom(string:lowercase(Method), utf8),

    case httpc:request(MethodAtom, Request, [{timeout, 30000}], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            {ok, StatusCode, RespHeaders, RespBody};
        {error, Reason} ->
            {error, Reason}
    end.
