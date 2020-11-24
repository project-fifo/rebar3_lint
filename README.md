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
output format may be configured separately in the `rebar.config`

[### This is the default if no config is provided ###](src/rebar3_lint_prv.erl#L86-L105)
```erlang
{elvis,
    [#{dirs => ["apps/*/src", "src"],
       filter => "*.erl",
       ruleset => erl_files
      },
     #{dirs => ["."],
       filter => "Makefile",
       ruleset => makefiles
      },
     #{dirs => ["."],
       filter => "rebar.config",
       ruleset => rebar_config
      }
     #{dirs => ["."],
       filter => "elvis.config",
       ruleset => elvis_config
      }
    ]
}
```
