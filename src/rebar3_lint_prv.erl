-module('rebar3_lint_prv').

-export([init/1, do/1, format_error/1]).

-behaviour(provider).

-define(PROVIDER, 'lint').
-define(DEPS, [compile, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},            % The 'user friendly' name of the task
                                 {module, ?MODULE},            % The module implementation of the task
                                 {bare, true},                 % The task can be run by the user, always true
                                 {deps, ?DEPS},                % The list of dependencies
                                 {example, "rebar3 lint"},     % How to use the plugin
                                 {opts, []},                   % list of options understood by the plugin
                                 {short_desc, "A rebar plugin for elvis"},
                                 {desc, "A rebar linter plugin based on elvis"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Elvis = get_elvis_config(State),
    case elvis_core:rock(Elvis) of
        ok ->
            {ok, State};
        {fail, _} ->
            {error, "Linting failed"}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec get_elvis_config(rebar_state:t()) -> elvis_config:config().
get_elvis_config(State) ->
    try_elvis_config_rebar(State).

-spec try_elvis_config_rebar(rebar_state:t()) -> elvis_config:config().
try_elvis_config_rebar(State) ->
    rebar_api:debug("Looking for Elvis in rebar.config", []),
    case rebar_state:get(State, elvis, no_config) of
        no_config ->
            try_elvis_config_file(State);
        Config ->
            Config
    end.

-spec try_elvis_config_file(rebar_state:t()) -> elvis_config:config().
try_elvis_config_file(State) ->
    Filename = filename:join(rebar_dir:root_dir(State), "elvis.config"),
    rebar_api:debug("Looking for Elvis in ~s", [Filename]),
    try
        elvis_config:load_file(Filename)
    catch
        throw:enoent ->
            default();
        throw:Error ->
            rebar_api:abort("Error reading Elvis config from ~s: ~p", [Filename, Error])
    end.

-spec default() -> elvis_config:config().
default() ->
    rebar_api:debug("Using default Elvis configuration", []),
    [#{dirs => ["apps/*/src", "src"],
       filter => "*.erl",
       rules => [{elvis_style, line_length,
                  #{ignore => [],
                    limit => 80,
                    skip_comments => false}},
                 {elvis_style, no_tabs},
                 {elvis_style, no_trailing_whitespace},
                 {elvis_style, macro_names, #{ignore => []}},
                 {elvis_style, macro_module_names},
                 {elvis_style, operator_spaces, #{rules => [{right, ","},
                                                            {right, "++"},
                                                            {left, "++"}]}},
                 {elvis_style, nesting_level, #{level => 3}},
                 {elvis_style, god_modules,
                  #{limit => 25,
                    ignore => []}},
                 {elvis_style, no_if_expression},
                 {elvis_style, invalid_dynamic_call,
                  #{ignore => []}},
                 {elvis_style, used_ignored_variable},
                 {elvis_style, no_behavior_info},
                 {
                   elvis_style,
                   module_naming_convention,
                   #{regex => "^[a-z]([a-z0-9]*_?)*(_SUITE)?$",
                     ignore => []}
                 },
                 {
                   elvis_style,
                   function_naming_convention,
                   #{regex => "^([a-z][a-z0-9]*_?)*$"}
                 },
                 {elvis_style, state_record_and_type},
                 {elvis_style, no_spec_with_records},
                 {elvis_style, dont_repeat_yourself, #{min_complexity => 10}},
                 {elvis_style, no_debug_call, #{ignore => []}}
                ]
      },
     #{dirs => ["."],
       filter => "Makefile",
       rules => [{elvis_project, no_deps_master_erlang_mk, #{ignore => []}},
                 {elvis_project, protocol_for_deps_erlang_mk, #{ignore => []}}]
      },
     #{dirs => ["."],
       filter => "rebar.config",
       rules => [{elvis_project, no_deps_master_rebar, #{ignore => []}},
                 {elvis_project, protocol_for_deps_rebar, #{ignore => []}}]
      }
    ].
