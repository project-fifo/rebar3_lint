-module('rebar3_lint_prv').

-export([init/1, do/1, format_error/1]).
-export([default_config/0]).

-ignore_xref([do/1]).
-ignore_xref([format_error/1]).

-define(PROVIDER, 'lint').
-define(DEPS, [compile, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create(
          [
           {name, ?PROVIDER},
           {module, ?MODULE},
           {bare, true},
           {deps, ?DEPS},
           {example, "rebar3 lint"},
           {opts, []},
           {short_desc, "A rebar plugin for elvis"},
           {desc, "A rebar linter plugin based on elvis"}
          ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Elvis = get_elvis_config(State),
    _ = rebar_log:log(info, "elvis analysis starting, this may take a while...", []),
    try elvis_core:rock(Elvis) of
        ok ->
            {ok, State};
        {fail, _} ->
            {error, "Linting failed"}
    catch throw:Error ->
        rebar_api:abort("elvis_core threw an exception: ~p", [Error])
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec get_elvis_config(rebar_state:t()) -> elvis_config:configs().
get_elvis_config(State) ->
    try_elvis_config_rebar(State).

-spec try_elvis_config_rebar(rebar_state:t()) -> elvis_config:configs().
try_elvis_config_rebar(State) ->
    rebar_api:debug("Looking for Elvis in rebar.config", []),
    handle_output_format(State),
    case rebar_state:get(State, elvis, no_config) of
        no_config ->
            try_elvis_config_file(State);
        Config ->
            Config
    end.

-spec handle_output_format(rebar_state:t()) -> ok.
handle_output_format(State) ->
    case rebar_state:get(State, elvis_output_format, no_config) of
        no_config -> ok;
        plain ->
            application:set_env(elvis_core, output_format, plain);
        colors ->
            application:set_env(elvis_core, output_format, colors);
        parsable ->
            application:set_env(elvis_core, output_format, parsable);
        Other ->
            rebar_api:abort("~p is not a valid elvis output format. Must be either plain, colors or"
                            "parsable", [Other])
    end.

-spec try_elvis_config_file(rebar_state:t()) -> elvis_config:configs().
try_elvis_config_file(State) ->
    Filename = filename:join(rebar_dir:root_dir(State), "elvis.config"),
    rebar_api:debug("Looking for Elvis in ~s", [Filename]),
    try elvis_config:from_file(Filename) of
        [] ->
            rebar_api:debug("Using default Elvis configuration", []),
            default_config();
        Config -> Config
    catch
        throw:Error ->
            rebar_api:abort("Error reading Elvis config from ~s: ~p",
                            [Filename, Error])
    end.

-spec default_config() -> elvis_config:configs().
default_config() ->
    [#{ dirs => ["apps/*/src/**", "src/**"],
        filter => "*.erl",
        ruleset => erl_files },
     #{ dirs => ["."],
        filter => "rebar.config",
        ruleset => rebar_config },
     #{ dirs => ["."],
        filter => "elvis.config",
        ruleset => elvis_config }].
