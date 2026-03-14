-module(rebar3_lint_prv).

%% The rebar3 plugin architecture has a poorly defined "behaviour"
-elvis([{elvis_style, consistent_ok_error_spec, disable}]).

-export([init/1, do/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, lint},
            {module, ?MODULE},
            {bare, true},
            {deps, [compile, app_discovery]},
            {example, "rebar3 lint"},
            {opts, []},
            {short_desc, "A rebar3 plugin for Elvis"},
            {desc, "A rebar3 plugin for Elvis, the Erlang linter"}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    _ = application:load(rebar3_lint),
    _ = application:load(elvis_core),
    _ = elvis_utils:info("analysis starting, this may take a while...", []),
    case elvis_core:rock() of
        ok ->
            {ok, State};
        {errors, _} ->
            {error, "Elvis: linting failed"};
        {warnings, _} ->
            {ok, State}
    end.
