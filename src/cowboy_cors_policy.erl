%% @doc CORS Policy definition and implementation.
-module(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allow_credentials/2]).
-export([exposed_headers/2]).

%% Pre-flight specific callbacks.
-export([allowed_methods/2]).
-export([allowed_headers/2]).
-export([max_age/2]).


-type state() :: any().
-type req()   :: cowboy_req:req().

-callback policy_init(Req) -> {ok, Req, state()} when Req :: cowboy_req:req().

-spec policy_init(req()) -> {ok, req(), state()}.
policy_init(Req) ->
    {ok, Req, undefined}.

%% @doc
%%  Return whitelist origins.
%% @end
-spec allowed_origins(req(), state()) -> {'*' | [binary()], state()}.
allowed_origins(_, State) ->
    ValidOrigins = application:get_env(cowboy_cors, origin_whitelist, []),
    {ValidOrigins, State}.

%% @doc
%%  Indicates which headers can be exposed as part of the response.
%%  Headers are read from configuration.
%% @end
-spec exposed_headers(req(), state()) -> {[binary()], state()}.
exposed_headers(_, State) ->
    ExposedHeaders = application:get_env(cowboy_cors, exposed_headers, []),
    {ExposedHeaders, State}.

%% @doc
%%  Returns headers that are allowed to be passed in a pre-flight request.
%% @end
-spec allowed_headers(req(), state()) -> {[binary()], state()}.
allowed_headers(_, State) ->
    AllowedHeaders = application:get_env(cowboy_cors, allowed_headers, []),
    {AllowedHeaders, State}.

-spec allow_credentials(req(), state()) -> {boolean(), state()}.
allow_credentials(_, State) ->
    Credential = application:get_env(cowboy_cors, allowed_credential, false),
    {Credential, State}.

-spec allowed_methods(req(), state()) -> {[binary()], state()}.
allowed_methods(_, State) ->
    AllowedMethods = application:get_env(cowboy_cors, allowed_methods, []),
    {AllowedMethods, State}.

-spec max_age(req(), state()) -> {non_neg_integer() | undefined, state()}.
max_age(_, State) ->
    MaxAgeSecs = application:get_env(cowboy_cors, max_age, undefined),
    {MaxAgeSecs, State}.
