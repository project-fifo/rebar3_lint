-module(test_app_SUITE).

-export([all/0, test_app/1]).

-behaviour(ct_suite).

all() ->
    [test_app].

test_app(_Config) ->
    ok = file:set_cwd("../../../../"),
    {ok, GoodState} =
        rebar3_lint_prv:init(
            rebar_state:new()),
    {ok, _} = rebar3_lint_prv:do(GoodState),
    BadState =
        rebar_state:set(GoodState,
                        elvis,
                        [#{dirs => ["src"],
                           filter => "*.erl",
                           rules => [{elvis_style, max_function_length, #{max_length => 1}}]}]),
    {error, {Message = "Linting failed with ~p", Reason}} = rebar3_lint_prv:do(BadState),
    % Check that this doesn't throw
    io:format("~p", [rebar3_lint_prv:format_error({Message, Reason})]).
