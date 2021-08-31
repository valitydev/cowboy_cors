-module(cors_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% ct callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% tests
-export([standard_no_origin_get/1]).
-export([standard_no_origin_options/1]).
-export([standard_get/1]).
-export([standard_options/1]).
-export([simple_allowed_get/1]).
-export([simple_wildcard_get/1]).
-export([simple_allowed_credentials_get/1]).
-export([simple_allowed_credentials_with_wildcard_origin/1]).
-export([simple_exposed_headers/1]).
-export([actual_options/1]).
-export([preflight_method/1]).
-export([preflight_allowed_method/1]).
-export([preflight_credentials/1]).
-export([preflight_wildcard_origin/1]).
-export([preflight_credentials_with_wildcard_origin/1]).
-export([preflight_header/1]).
-export([preflight_allowed_header/1]).
-export([preflight_allowed_header_webkit/1]).
-export([preflight_max_age/1]).
-export([preflight_invalid_max_age/1]).
%% Config cors policy
-export([origin_whitelist_allowed_all/1]).
-export([origin_whitelist_unavail/1]).
-export([origin_whitelist_not_member/1]).
-export([credential_enabled/1]).
-export([request_headers_not_allowed/1]).
-export([request_methods_not_allowed/1]).
-export([exposed_headers_config/1]).
-export([max_age_enabled/1]).

all() ->
    [
        {group, default},
        {group, policy},
        {group, cors_config_policy}
    ].

groups() ->
    [
        {default, [parallel], [
            standard_no_origin_get,
            standard_no_origin_options,
            standard_get,
            standard_options
        ]},
        {policy, [parallel], [
            standard_no_origin_get,
            standard_no_origin_options,
            standard_get,
            standard_options,
            simple_allowed_get,
            simple_wildcard_get,
            simple_allowed_credentials_get,
            simple_allowed_credentials_with_wildcard_origin,
            simple_exposed_headers,
            actual_options,
            preflight_method,
            preflight_allowed_method,
            preflight_credentials,
            preflight_wildcard_origin,
            preflight_credentials_with_wildcard_origin,
            preflight_header,
            preflight_allowed_header,
            preflight_allowed_header_webkit,
            preflight_max_age,
            preflight_invalid_max_age
        ]},
        {cors_config_policy, [], [
            standard_no_origin_get,
            standard_no_origin_options,
            standard_get,
            standard_options,
            % Origin whitelist
            origin_whitelist_allowed_all,
            origin_whitelist_unavail,
            origin_whitelist_not_member,
            credential_enabled,
            request_headers_not_allowed,
            request_methods_not_allowed,
            exposed_headers_config,
            max_age_enabled
        ]}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(cowboy_cors),
    application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy_cors),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    application:stop(gun),
    ok.

init_per_group(Name, Config) ->
    Policy =
        case Name of
            default -> cors_default_policy;
            policy -> cors_policy;
            cors_config_policy -> cowboy_cors_policy
        end,

    Middlewares = [cowboy_cors, cowboy_handler],
    Env = #{
        handler => cors_handler,
        handler_opts => [],
        cors_policy => Policy
    },
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{env => Env, middlewares => Middlewares}),
    Port = ranch:get_port(Name),
    [{port, Port} | Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

%% Helpers

build_url(Host, Port, Path, Options) ->
    Params = build_params(Options),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), Path, Params]).

build_params(Options) ->
    case lists:map(fun build_param/1, Options) of
        [] ->
            [];
        [["&" | First] | Rest] ->
            ["?", First, Rest]
    end.

build_param({Name, Value}) ->
    ["&", atom_to_list(Name), "=", format_option(Value)].

format_option(Bin) when is_binary(Bin) ->
    Bin;
format_option(Value) when
    is_boolean(Value);
    is_integer(Value)
->
    io_lib:fwrite("~p", [Value]);
format_option(List) when is_list(List) ->
    IoList = lists:map(fun(X) -> [",", X] end, List),
    <<",", Bin/binary>> = iolist_to_binary(IoList),
    Bin.

request(Method, Headers, Options, Config) ->
    Port = ?config(port, Config),
    Host = "localhost",
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(ConnPid),
    RequestUrl = build_url(Host, Port, <<"/">>, Options),
    gun:request(ConnPid, Method, RequestUrl, Headers, []),
    receive
        {gun_response, ConnPid, _, fin, Status, Resp} ->
            {ok, Status, Resp};
        {'DOWN', _, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after 1000 -> exit(timeout)
    end.

request(Method, Headers, Config) ->
    request(Method, Headers, [], Config).

get_all_env() ->
    application:get_all_env(cowboy_cors).

set_all_env(Params) ->
    lists:foreach(
        fun({Key, Value}) ->
            application:set_env(cowboy_cors, Key, Value)
        end,
        Params
    ).
%% Tests

standard_no_origin_get(Config) ->
    {ok, 200, Headers} = request(<<"GET">>, [], Config),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)).

standard_no_origin_options(Config) ->
    {ok, 200, Headers} = request(<<"OPTIONS">>, [], Config),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)).

standard_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} = request(<<"GET">>, [{<<"Origin">>, Origin}], Config),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)).

standard_options(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} = request(<<"OPTIONS">>, [{<<"Origin">>, Origin}], Config),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)).

simple_allowed_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [
                {allowed_origins, [<<"http://example.org">>, Origin]},
                {allowed_methods, [<<"PUT">>, <<"GET">>]}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)).

simple_wildcard_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [{allowed_origins, "*"}],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)).

simple_allowed_credentials_get(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [
                {allowed_origins, Origin},
                {allow_credentials, true}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"true">>, get_header(<<"access-control-allow-credentials">>, Headers)).

simple_allowed_credentials_with_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [
                {allowed_origins, "*"},
                {allow_credentials, true}
            ],
            Config
        ),
    %% We MUST not see "*" as the value for this header if credentials are allowed.
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"true">>, get_header(<<"access-control-allow-credentials">>, Headers)).

simple_exposed_headers(Config) ->
    Origin = <<"http://example.com">>,
    Exposed = [<<"x-first">>, <<"x-second">>],
    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"GET">>},
                {exposed_headers, Exposed}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(Exposed, get_expose_headers(Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)).

actual_options(Config) ->
    %% OPTIONS request without Access-Control-Request-Method is not a pre-flight request.
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [{<<"Origin">>, Origin}],
            [{allowed_origins, Origin}],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    %% Ensure OPTIONS request was handled.
    ?assertEqual(<<"exposed">>, get_header(<<"x-exposed">>, Headers)).

preflight_method(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"DELETE">>}
            ],
            [{allowed_origins, Origin}],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_allowed_method(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, [<<"GET">>, <<"PUT">>]}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    Allowed = get_header(<<"access-control-allow-methods">>, Headers),
    true = lists:member(<<"GET">>, binary:split(Allowed, <<",">>)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    ?assert(not has_header(<<"access-control-max-age">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_credentials(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {allow_credentials, true}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assertEqual(<<"true">>, get_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, "*"},
                {allowed_methods, <<"PUT">>}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    ?assert(not has_header(<<"access-control-max-age">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_credentials_with_wildcard_origin(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, "*"},
                {allow_credentials, true},
                {allowed_methods, <<"PUT">>}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assertEqual(<<"true">>, get_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    ?assert(not has_header(<<"access-control-max-age">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_header(Config) ->
    Origin = <<"http://example.com">>,
    AllowedHeaders = [<<"x-unused">>, <<"x-also-unused">>],
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Custom">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {allowed_headers, AllowedHeaders}
            ],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-headers">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_allowed_header(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {allowed_headers, [<<"x-allowed">>, <<"x-requested">>]}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(access_control_header_allowed(<<"x-requested">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

%% Test for Webkit browsers requesting 'Origin' header.
preflight_allowed_header_webkit(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"origin, x-requested">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {allowed_headers, [<<"x-allowed">>, <<"x-requested">>]}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(access_control_header_allowed(<<"x-requested">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)).

preflight_max_age(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {max_age, 30}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"30">>, get_header(<<"access-control-max-age">>, Headers)).

% if max age is negative, middleware IS TO FAIL

preflight_invalid_max_age(Config) ->
    Origin = <<"http://example.com">>,
    {ok, 500, _} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>}
            ],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {max_age, -30}
            ],
            Config
        ).

%% Test for configuration cors policy
origin_whitelist_allowed_all(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    %% Set origin to '*'
    ok = application:set_env(cowboy_cors, origin_whitelist, '*'),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    ok = application:set_env(cowboy_cors, allowed_credential, false),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(access_control_header_allowed(<<"x-requested">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    ?assert(not has_header(<<"access-control-expose-headers">>, Headers)),
    %% Pre-flight requests should not be completed by the handler.
    ?assert(not has_header(<<"x-exposed">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

origin_whitelist_unavail(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    %% Set origin to []
    ok = application:set_env(cowboy_cors, origin_whitelist, []),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-headers">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

origin_whitelist_not_member(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    ok = application:set_env(cowboy_cors, origin_whitelist, [<<"http://exam.com">>]),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-headers">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

credential_enabled(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    %% Set origin to '*'
    ok = application:set_env(cowboy_cors, origin_whitelist, '*'),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    ok = application:set_env(cowboy_cors, allowed_credential, true),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"PUT">>, get_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(access_control_header_allowed(<<"x-requested">>, Headers)),
    ?assertEqual(<<"true">>, get_header(<<"access-control-allow-credentials">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

request_headers_not_allowed(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    ok = application:set_env(cowboy_cors, origin_whitelist, '*'),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"authorization">>]),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-headers">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

request_methods_not_allowed(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    %% Set origin to '*'
    ok = application:set_env(cowboy_cors, origin_whitelist, '*'),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"GET">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assert(not has_header(<<"access-control-allow-origin">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-methods">>, Headers)),
    ?assert(not has_header(<<"access-control-allow-headers">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

exposed_headers_config(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    Origin = <<"http://example.com">>,
    Exposed = [<<"x-first">>, <<"x-second">>],
    ok = application:set_env(cowboy_cors, origin_whitelist, [Origin]),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, exposed_headers, Exposed),

    {ok, 200, Headers} =
        request(
            <<"GET">>,
            [{<<"Origin">>, Origin}],
            [
                {allowed_origins, Origin},
                {allowed_methods, <<"PUT">>},
                {exposed_headers, [<<"x-second">>]}
            ],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(Exposed, get_expose_headers(Headers)),
    ?assert(not has_header(<<"access-control-allow-credentials">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

max_age_enabled(Config) ->
    %% Preconfig
    OrigEnv = get_all_env(),
    %% Set origin to '*'
    ok = application:set_env(cowboy_cors, origin_whitelist, '*'),
    ok = application:set_env(cowboy_cors, allowed_methods, [<<"PUT">>]),
    ok = application:set_env(cowboy_cors, allowed_headers, [<<"x-requested">>]),
    ok = application:set_env(cowboy_cors, max_age, 30),
    Origin = <<"http://example.com">>,
    {ok, 200, Headers} =
        request(
            <<"OPTIONS">>,
            [
                {<<"Origin">>, Origin},
                {<<"Access-Control-Request-Method">>, <<"PUT">>},
                {<<"Access-Control-Request-Headers">>, <<"X-Requested">>}
            ],
            [],
            Config
        ),
    ?assertEqual(Origin, get_header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"30">>, get_header(<<"access-control-max-age">>, Headers)),
    %% Postconfig
    set_all_env(OrigEnv).

get_header(Header, Headers) ->
    {Header, Value} = lists:keyfind(Header, 1, Headers),
    Value.

has_header(Header, Headers) ->
    false /= lists:keyfind(Header, 1, Headers).

get_expose_headers(Headers) ->
    ExposedList = get_header(<<"access-control-expose-headers">>, Headers),
    cowboy_cors_utils:nonempty_list(ExposedList, fun cowboy_cors_utils:token/2).

access_control_header_allowed(Header, Headers) ->
    case lists:keyfind(<<"access-control-allow-headers">>, 1, Headers) of
        {_, AllowedList} ->
            lists:member(Header, binary:split(AllowedList, <<",">>, [global]));
        false ->
            false
    end.
