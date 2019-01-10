%% @doc Cross-Origin Resource Sharing (CORS) middleware.
%%
%% Policy is defined through callbacks contained in a module named by
%% the <em>cors_policy</em> environment value.
%%
%% @see http://www.w3.org/TR/cors/
-module(cowboy_cors).
-behaviour(cowboy_middleware).

-export([execute/2]).

-record(state, {
          env                  :: cowboy_middleware:env(),
          method               :: binary(),
          origin               :: binary(),
          request_method       :: binary(),
          request_headers = [] :: [binary()],
          preflight = false    :: boolean(),

          %% Policy handler.
          policy               :: atom(),
          policy_state         :: any()
}).

%% @private
execute(Req, Env) ->
    ct:log("execute"),
    Policy = maps:get(cors_policy, Env),
    Method = cowboy_req:method(Req),
    origin_present(Req, #state{env = Env, policy = Policy, method = Method}).

%% CORS specification only applies to requests with an `Origin' header.
origin_present(Req, State) ->
    ct:log("Origin present"),
    case cowboy_req:header(<<"origin">>, Req) of
        undefined ->
            terminate(Req, State);
        Origin ->
            policy_init(Req, State#state{origin = Origin})
    end.

policy_init(Req, State = #state{policy = Policy}) ->
    try Policy:policy_init(Req) of
        {ok, Req1, PolicyState} ->
            ct:pal("~p, trying to call ~p:policy_init", [?FUNCTION_NAME, Policy]),
            Res = allowed_origins(Req1, State#state{policy_state = PolicyState}),
            ct:pal("called policy_init successfuly"),
            Res
    catch Class:Reason:Stacktrace ->
                error_logger:error_msg(
                  "** Cowboy CORS policy ~p terminating in ~p/~p~n"
                  "   for the reason ~p:~p~n"
                  "** Request was ~p~n** Stacktrace: ~p~n~n",
                  [Policy, policy_init, 1, Class, Reason,
                   Req, Stacktrace]),
            error_terminate(Req, State)
    end.

%% allowed_origins/2 should return a list of origins or the atom '*'
allowed_origins(Req, State = #state{origin = Origin}) ->
    case call(Req, State, allowed_origins, []) of
        {'*', PolicyState} ->
            request_method(Req, State#state{policy_state = PolicyState});
        {List, PolicyState} ->
            case lists:member(Origin, List) of
                true ->
                    ct:pal("Origin is here"),
                    request_method(Req, State#state{policy_state = PolicyState});
                false ->
                    ct:pal("Origin is not here"),
                    terminate(Req, State#state{policy_state = PolicyState})
            end
    end.

request_method(Req, State = #state{method = <<"OPTIONS">>}) ->
    case cowboy_req:header(<<"access-control-request-method">>, Req) of
        undefined ->
            %% This is not a pre-flight request, but an actual request.
            exposed_headers(Req, State);
        Data ->
            token(Data,
                              fun(<<>>, Method) ->
                                      request_headers(Req, State#state{preflight = true,
                                                                        request_method = Method});
                                 (_, _) ->
                                      terminate(Req, State)
                              end)
    end;
request_method(Req, State) ->
    exposed_headers(Req, State).

token(Data, Fun) ->
	token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
	Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
		when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
			 C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
			 C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
			 C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
			 C < 32; C =:= 127 ->
	Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
	C2 = string:to_lower(C),
	token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
	token(Rest, Fun, Case, << Acc/binary, C >>).


request_headers(Req, State) ->
    Headers = cowboy_req:header(<<"access-control-request-headers">>, Req, <<>>),
    case cowboy_http:list(Headers, fun cowboy_http:token_ci/2) of
        {error, badarg} ->
            terminate(Req, State);
        List ->
            max_age(Req, State#state{request_headers = List})
    end.

%% max_age/2 should return a non-negative integer or the atom undefined
max_age(Req, State) ->
    case call(Req, State, max_age, undefined) of
        {MaxAge, Req1, PolicyState}
          when is_integer(MaxAge) andalso MaxAge >= 0 ->
            MaxAgeBin = list_to_binary(integer_to_list(MaxAge)),
            Req2 = cowboy_req:set_resp_header(<<"access-control-max-age">>, MaxAgeBin, Req1),
            allowed_methods(Req2, State#state{policy_state = PolicyState});
        {undefined, Req1, PolicyState} ->
            allowed_methods(Req1, State#state{policy_state = PolicyState})
    end.

%% allow_methods/2 should return a list of binary method names
allowed_methods(Req, State = #state{request_method = Method}) ->
    {List, Req1, PolicyState} = call(Req, State, allowed_methods, []),
    case lists:member(Method, List) of
        false ->
            terminate(Req1, State#state{policy_state = PolicyState});
        true ->
            allowed_headers(Req1, State#state{policy_state = PolicyState})
    end.

allowed_headers(Req, State = #state{request_headers = Requested}) ->
    {List, Req1, PolicyState} = call(Req, State, allowed_headers, []),
    check_allowed_headers(Requested, List, Req1, State#state{policy_state = PolicyState}).

check_allowed_headers([], _, Req, State) ->
    set_allow_methods(Req, State);
check_allowed_headers([<<"origin">>|Tail], Allowed, Req, State) ->
    %% KLUDGE: for browsers that include this header, but don't
    %% actually check it (i.e. Webkit).  Given that the 'Origin'
    %% header underpins the entire CORS framework, its inclusion in
    %% the requested headers is nonsensical.
    check_allowed_headers(Tail, Allowed, Req, State);
check_allowed_headers([Header|Tail], Allowed, Req, State) ->
    case lists:member(Header, Allowed) of
        false ->
            terminate(Req, State);
        true ->
            check_allowed_headers(Tail, Allowed, Req, State)
    end.

set_allow_methods(Req, State = #state{request_method = Method}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Method, Req),
    set_allow_headers(Req1, State).

set_allow_headers(Req, State) ->
    %% Since we have already validated the requested headers, we can
    %% simply reflect the list back to the client.
    case cowboy_req:header(<<"access-control-request-headers">>, Req) of
        {undefined, Req1} ->
            allow_credentials(Req1, State);
        {Headers, Req1} ->
            Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req1),
            allow_credentials(Req2, State)
    end.

%% exposed_headers/2 should return a list of binary header names.
exposed_headers(Req, State) ->
    {List, Req1, PolicyState} = call(Req, State, exposed_headers, []),
    Req2 = set_exposed_headers(Req1, List),
    allow_credentials(Req2, State#state{policy_state = PolicyState}).

set_exposed_headers(Req, []) ->
    Req;
set_exposed_headers(Req, Headers) ->
    Bin = header_list(Headers),
    cowboy_req:set_resp_header(<<"access-control-expose-headers">>, Bin, Req).

%% allow_credentials/1 should return true or false.
allow_credentials(Req, State) ->
    expect(Req, State, allow_credentials, false,
           fun if_not_allow_credentials/2, fun if_allow_credentials/2).

%% If credentials are allowed, then the value of
%% `Access-Control-Allow-Origin' is limited to the requesting origin.
if_allow_credentials(Req, State = #state{origin = Origin}) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req2),
    terminate(Req3, State).

if_not_allow_credentials(Req, State = #state{origin = Origin}) ->
    %% To simplify conformance with the requirement that "*" is only
    %% valid if credentials are not allowed, we will only ever return
    %% the origin of the request.
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req2 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req1),
    terminate(Req2, State).

expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
    case call(Req, State, Callback, Expected) of
        {Expected, Req1, PolicyState} ->
            OnTrue(Req1, State#state{policy_state = PolicyState});
        {_Unexpected, Req1, PolicyState} ->
            OnFalse(Req1, State#state{policy_state = PolicyState})
    end.

call(Req, State = #state{policy = Policy, policy_state = PolicyState}, Callback, Default) ->
    case erlang:function_exported(Policy, Callback, 2) of
        true ->
            try
                ct:pal("~p ~p:~p ~nReq: ~p ~n PolicyState: ~p", [?FUNCTION_NAME, Policy, Callback, Req, PolicyState]),
                Res = Policy:Callback(Req, PolicyState),
                ct:pal("~p ~p:~p success, res: ~p", [?FUNCTION_NAME, Policy, Callback, Res]),
                Res
            catch Class:Reason:Stacktrace ->
                    error_logger:error_msg(
                      "** Cowboy CORS policy ~p terminating in ~p/~p~n"
                      "   for the reason ~p:~p~n"
                      "** Request was ~p~n** Stacktrace: ~p~n~n",
                      [Policy, Callback, 2, Class, Reason,
                       Req, Stacktrace]),
                    error_terminate(Req, State)
            end;
        false ->
            ct:pal("Function ~p:~p is not exported", [Policy, Callback]),
            {Default, PolicyState}
    end.

terminate(Req, #state{preflight = true}) ->
    {error, 200, Req};
terminate(Req, #state{env = Env}) ->
    {ok, Req, Env}.

-spec error_terminate(cowboy_req:req(), #state{}) -> no_return().
error_terminate(_Req, _State) ->
    erlang:throw({?MODULE, error}).

%% create a comma-separated list for a header value
header_list(Values) ->
    header_list(Values, <<>>).

header_list([Value], Acc) ->
    <<Acc/binary, Value/binary>>;
header_list([Value | Rest], Acc) ->
    header_list(Rest, <<Acc/binary, Value/binary, ",">>).
