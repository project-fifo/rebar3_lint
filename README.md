rebar3_lint
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{profiles,
  [
   {lint,  [{plugins, [{rebar3_lint, {git, "https://github.com/project-fifo/rebar3_lint.git", {tag, "0.1.11"}}}]}]}
  ]
}.
```

Then just call your plugin directly in an existing application:


    $ rebar3 as lint lint
    ===> Fetching rebar3_lint
    ===> Compiling rebar3_lint
    <Plugin Output>

## Configuration ##

the plugin supports the following configuration options in the rebar.config:

Elvis check configuration (keyword config if placed in application config or
elvis.config file)

```erlang
{elvis, [map()]}.
```

Specify output format. Default: colors

```erlang
{elvis_output_format, plain | colors}.
```

If no `elvis` configuation statement is given in the `rebar.config` file the
plug-in will look for a `elvis.config` file in the project root folder. But
only the config section will be applied (this is an elvis idiosyncrasy). The
outpup format may be configured separately in the `rebar.config`

### This is the default if no config is provided ###
```erlang
{elvis,
 [#{dirs => ["apps/*/src", "src"],
       filter => "*.erl",
       rules => [{elvis_style, line_length,
                  #{ignore => [],
                    limit => 100,
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
    ]
}
```
