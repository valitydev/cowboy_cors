-module(cowboy_cors_utils).

% This module  mostly constains copy-pasted functions, that went missing from cowboy since 0.8.6
% version, yet still used in this app.

-export([nonempty_list/2]).
-export([list/2]).
-export([token_ci/2]).
-export([token/2]).

-spec nonempty_list(binary(), fun()) -> [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        [] -> {error, badarg};
        L -> lists:reverse(L)
    end.

-spec list(binary(), fun()) -> list() | {error, badarg}.
list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        L -> lists:reverse(L)
    end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
list(Data, Fun, Acc) ->
    whitespace(
        Data,
        fun(Str) -> list_first(Str, Fun, Acc) end
    ).

list_first(<<"">>, _Fun, Acc) ->
    Acc;
list_first(<<$,, Rest/binary>>, Fun, Acc) ->
    list(Rest, Fun, Acc);
list_first(Rest, Fun, Acc) ->
    Fun(
        Rest,
        fun(D, I) ->
            whitespace(
                D,
                fun(Next) -> list_next(Next, I, Fun, Acc) end
            )
        end
    ).

list_next(<<>>, Token, _, Acc) -> [Token | Acc];
list_next(<<$,, R/binary>>, Token, Fun, Acc) -> list(R, Fun, [Token | Acc]);
list_next(_Any, _, _, _) -> {error, badarg}.

-spec whitespace(binary(), fun()) -> any().
whitespace(<<C, Rest/binary>>, Fun) when C =:= $\s; C =:= $\t ->
    whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).

-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
    token(Data, Fun, ci, <<>>).

-spec token(binary(), fun()) -> any().
token(Data, Fun) ->
    token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
    Fun(<<>>, Acc);
token(Data = <<C, _Rest/binary>>, Fun, _Case, Acc) when
    C =:= $(;
    C =:= $);
    C =:= $<;
    C =:= $>;
    C =:= $@;
    C =:= $,;
    C =:= $;;
    C =:= $:;
    C =:= $\\;
    C =:= $";
    C =:= $/;
    C =:= $[;
    C =:= $];
    C =:= $?;
    C =:= $=;
    C =:= ${;
    C =:= $};
    C =:= $\s;
    C =:= $\t;
    C < 32;
    C =:= 127
->
    Fun(Data, Acc);
token(<<C, Rest/binary>>, Fun, Case = ci, Acc) ->
    C2 = string:to_lower(C),
    token(Rest, Fun, Case, <<Acc/binary, C2>>);
token(<<C, Rest/binary>>, Fun, Case, Acc) ->
    token(Rest, Fun, Case, <<Acc/binary, C>>).
