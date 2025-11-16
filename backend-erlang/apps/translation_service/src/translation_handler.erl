%%%-------------------------------------------------------------------
%%% @doc Translation Service HTTP Handler
%%% Handles translation requests
%%% @end
%%%-------------------------------------------------------------------
-module(translation_handler).

-export([init/2]).

init(Req0, #{action := translate} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            Text = maps:get(<<"text">>, Data),
            SourceLang = maps:get(<<"sourceLang">>, Data),
            TargetLang = maps:get(<<"targetLang">>, Data),

            %% Check cache first
            CacheKey = <<SourceLang/binary, ":", TargetLang/binary, ":", Text/binary>>,
            HashedKey = crypto:hash(md5, CacheKey),
            CacheKeyStr = binary:encode_hex(HashedKey),

            case tolkflip_redis:get(<<"translation:", CacheKeyStr/binary>>) of
                {ok, CachedTranslation} when CachedTranslation =/= undefined ->
                    %% Return cached translation
                    Reply = jsx:encode(#{
                        <<"translatedText">> => CachedTranslation,
                        <<"sourceLang">> => SourceLang,
                        <<"targetLang">> => TargetLang,
                        <<"cached">> => true
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                _ ->
                    %% Perform translation (mock implementation - integrate with real API)
                    TranslatedText = mock_translate(Text, SourceLang, TargetLang),

                    %% Cache result (24 hour TTL)
                    tolkflip_redis:setex(
                        <<"translation:", CacheKeyStr/binary>>,
                        86400,
                        TranslatedText
                    ),

                    Reply = jsx:encode(#{
                        <<"translatedText">> => TranslatedText,
                        <<"sourceLang">> => SourceLang,
                        <<"targetLang">> => TargetLang,
                        <<"cached">> => false
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"translation-service">>}),
        Req),
    {ok, Req2, State}.

%% Mock translation function - replace with real API integration
mock_translate(Text, _SourceLang, _TargetLang) ->
    %% TODO: Integrate with Google Translate API, DeepL, or similar
    <<"[Translated: ", Text/binary, "]">>.
