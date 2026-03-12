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
    ErrorOrOkElvisConfig =
        case elvis_config:config() of
            {error, Message0} ->
                {error, Message0};
            [] ->
                _ = elvis_utils:warn("Elvis: elvis.config not defined; using default", []),
                {ok, elvis_config:default()};
            ElvisConfig0 ->
                {ok, ElvisConfig0}
        end,
    case ErrorOrOkElvisConfig of
        {error, Message} ->
            {error, io_lib:format("Elvis: invalid configuration: ~s", [Message])};
        {ok, ElvisConfig} ->
            _ = elvis_utils:info("analysis starting, this may take a while...", []),
            case elvis_core:rock(ElvisConfig) of
                ok ->
                    {ok, State};
                {errors, _} ->
                    {error, "Elvis: linting failed"};
                {warnings, _} ->
                    {ok, State}
            end
    end.
