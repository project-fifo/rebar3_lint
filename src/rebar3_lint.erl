-module('rebar3_lint').

-export([init/1]).

-ignore_xref([init/1]).

%% for eating our own dogfood
-export([main/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_lint_prv:init(State),
    {ok, State1}.

%% @private
-spec main([]) -> ok | {fail, [elvis_result:file()]}.
main([]) ->
    ok = application:load(elvis_core),
    elvis_core:rock(elvis_config:from_rebar("rebar.config")).
