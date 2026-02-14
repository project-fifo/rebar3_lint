-module(test_app_SUITE).

-export([all/0, test_app/1]).

-behaviour(ct_suite).

all() ->
    [test_app].

test_app(_Config) ->
    ok = file:set_cwd("../../../../"),
    ct:log("Good State"),
    {ok, GoodState} =
        rebar3_lint_prv:init(
            rebar_state:new()
        ),
    {ok, _} = rebar3_lint_prv:do(GoodState),
    ct:log("Invalid State"),
    InvalidState =
        rebar_state:set(
            GoodState,
            elvis,
            [
                #{
                    dirs => ["bad-dir"],
                    filter => "*.erl",
                    ruleset => erl_files
                }
            ]
        ),
    try
        rebar3_lint_prv:do(InvalidState)
    catch
        throw:rebar_abort -> ok
    end,
    ct:log("NoConfig State"),
    NoConfigState = rebar_state:set(GoodState, elvis, no_config),
    try
        rebar3_lint_prv:do(NoConfigState)
    catch
        throw:rebar_abort -> ok
    end,
    ct:log("Bad State"),
    BadState =
        rebar_state:set(
            GoodState,
            elvis,
            [
                #{
                    dirs => ["src"],
                    filter => "*.erl",
                    rules => [{elvis_style, max_function_length, #{max_length => 1}}]
                }
            ]
        ),
    {error, "Linting failed"} = rebar3_lint_prv:do(BadState),
    {comment, ""}.
