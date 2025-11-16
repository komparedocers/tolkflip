%%%-------------------------------------------------------------------
%%% @doc JWT token generation and validation
%%% @end
%%%-------------------------------------------------------------------
-module(tolkflip_jwt).

-export([generate_access_token/2, generate_refresh_token/1, validate_token/1]).

%%====================================================================
%% API
%%====================================================================

generate_access_token(UserId, PhoneNumber) ->
    {ok, JwtConfig} = application:get_env(tolkflip_shared, jwt),
    Secret = proplists:get_value(secret, JwtConfig),
    Expiry = proplists:get_value(access_token_expiry, JwtConfig),

    Now = os:system_time(second),
    Claims = #{
        <<"userId">> => list_to_binary(UserId),
        <<"phoneNumber">> => list_to_binary(PhoneNumber),
        <<"type">> => <<"access">>,
        <<"iat">> => Now,
        <<"exp">> => Now + Expiry
    },

    jwerl:sign(Claims, hs256, Secret).

generate_refresh_token(UserId) ->
    {ok, JwtConfig} = application:get_env(tolkflip_shared, jwt),
    Secret = proplists:get_value(secret, JwtConfig),
    Expiry = proplists:get_value(refresh_token_expiry, JwtConfig),

    Now = os:system_time(second),
    Claims = #{
        <<"userId">> => list_to_binary(UserId),
        <<"type">> => <<"refresh">>,
        <<"iat">> => Now,
        <<"exp">> => Now + Expiry
    },

    jwerl:sign(Claims, hs256, Secret).

validate_token(Token) ->
    {ok, JwtConfig} = application:get_env(tolkflip_shared, jwt),
    Secret = proplists:get_value(secret, JwtConfig),

    case jwerl:verify(Token, hs256, Secret) of
        {ok, Claims} ->
            %% Check expiration
            case maps:get(<<"exp">>, Claims, undefined) of
                undefined ->
                    {error, invalid_token};
                Exp ->
                    Now = os:system_time(second),
                    if
                        Now < Exp ->
                            {ok, Claims};
                        true ->
                            {error, token_expired}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.
