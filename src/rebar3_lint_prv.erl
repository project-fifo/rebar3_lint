-module(rebar3_lint_prv).

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
            {fail, [{throw, {invalid_config, Message0}}]} ->
                % When we implement warnings_as_errors, revisit this
                % Maybe think about making this the default for `elvis_config:config()`
                % with an output notice
                elvis_utils:abort(Message0);
            [] ->
                elvis_utils:warn("Elvis: elvis.config not defined; using default", []),
                {ok, elvis_config:default()};
            ElvisConfig0 ->
                {ok, ElvisConfig0}
        end,
    case ErrorOrOkElvisConfig of
        {error, Message} ->
            elvis_utils:abort("invalid configuration: ~s", [Message]);
        {ok, ElvisConfig} ->
            _ = elvis_utils:info("analysis starting, this may take a while...", []),
            case elvis_core:rock(ElvisConfig) of
                ok ->
                    {ok, State};
                {fail, _} ->
                    elvis_utils:abort("linting failed", [])
            end
    end.
