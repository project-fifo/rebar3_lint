-module('rebar3_lint').

-export([init/1]).

-ignore_xref([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_lint_prv:init(State),
    {ok, State1}.
