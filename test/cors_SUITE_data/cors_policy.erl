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
    case parse_list(<<"allowed_origins">>, Req) of
        [<<"*">>] ->
            {'*', State};
        Allowed ->
            {Allowed, State}
    end.

allow_credentials(Req, State) ->
    IsAllowed = parse_boolean(<<"allow_credentials">>, Req, false),
    {IsAllowed, State}.

exposed_headers(Req, State) ->
    Exposed = parse_list(<<"exposed_headers">>, Req),
    {Exposed, State}.

allowed_headers(Req, State) ->
    Allowed = parse_list(<<"allowed_headers">>, Req),
    {Allowed, State}.

allowed_methods(Req, State) ->
    Allowed = parse_list(<<"allowed_methods">>, Req),
    {Allowed, State}.

max_age(Req, State) ->
    MaxAge = parse_integer(<<"max_age">>, Req),
    {MaxAge, State}.

parse_list(Name, Req) ->
    QSs = cowboy_req:parse_qs(Req),
    case lists:keyfind(Name, 1, QSs) of
        false ->
            [];
        {Name, Value} ->
           binary:split(Value, <<",">>, [global])
    end.

parse_boolean(Name, Req, Default) ->
    QSs = cowboy_req:parse_qs(Req),
    case lists:keyfind(Name, 1, QSs) of
        false ->
            Default;
        {Name, <<"true">>} ->
           true;
        {Name, <<"false">>} ->
            false
    end.

parse_integer(Name, Req) ->
    QSs = cowboy_req:parse_qs(Req),
    case lists:keyfind(Name, 1, QSs) of
        false ->
            undefined;
        {Name, Value} ->
            String = binary_to_list(Value),
            {MaxAge, []} = string:to_integer(String),
            MaxAge
    end.
