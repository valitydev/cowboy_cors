-module(cors_policy).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allow_credentials/2]).
-export([exposed_headers/2]).
-export([allowed_headers/2]).
-export([allowed_methods/2]).
-export([max_age/2]).

policy_init(Req) ->
    {ok, Req, undefined_state}.

allowed_origins(Req, State) ->
    case parse_list(allowed_origins, Req) of
        [<<"*">>] ->
            {'*', State};
        Allowed ->
            {Allowed, State}
    end.

allow_credentials(Req, State) ->
    IsAllowed = parse_boolean(allow_credentials, Req, false),
    {IsAllowed, State}.

exposed_headers(Req, State) ->
    Exposed = parse_list(exposed_headers, Req),
    {Exposed, State}.

allowed_headers(Req, State) ->
    Allowed = parse_list(allowed_headers, Req),
    {Allowed, State}.

allowed_methods(Req, State) ->
    Allowed = parse_list(allowed_methods, Req),
    {Allowed, State}.

max_age(Req, State) ->
    MaxAge = parse_integer(max_age, Req),
    {MaxAge, State}.

parse_list(Name, Req) ->
    #{Name := Value} = cowboy_req:match_qs([{Name, [nonempty], undefined}], Req),
    case Value of
        undefined ->
            [];
        _ ->
            binary:split(Value, <<",">>, [global])
    end.

parse_boolean(Name, Req, Default) ->
    #{Name := Value} = cowboy_req:match_qs([{Name, [nonempty], undefined}], Req),
    case  Value of
        <<"true">> -> true;
        <<"false">> -> false;
        undefined -> Default
    end.

parse_integer(Name, Req) ->
    #{Name := Value} = cowboy_req:match_qs([{Name, [int], undefined}], Req),
    Value.
