rebar3_lint
===

![build](https://github.com/project-fifo/rebar3_lint/workflows/build/badge.svg)

`rebar3_lint` is a `rebar3` plugin to ease the integration of
[elvis](https://github.com/inaka/elvis_core) into your project.

Use
---

Add the plugin to your `rebar.config`:

```erlang
{project_plugins, [
    rebar3_lint
]}.
```

Then call it:

```bash
$ rebar3 lint
===> Fetching rebar3_lint
===> Compiling rebar3_lint
<plugin output>
```

Configuration
---

The plugin supports configuration option `elvis_output_format` in `rebar.config`:

```erlang
{elvis_output_format, plain | colors | parsable}. % default: colors
```

It also supports option `elvis` (in `rebar.config`) as you'd find `elvis`'s own
`config` (inside option `elvis`).

If no `elvis` configuration is present in `rebar.config`, the
plug-in will look for an `elvis.config` file in the project root folder
(but only the `config` section will be applied - this is an `elvis` idiosyncrasy).

The output format may then be configured separately in `rebar.config`, as previously
explained.

This is the default configuration if no input is provided:

```erlang
[#{ dirs => ["apps/*/src/**", "src/**"],
    filter => "*.erl",
    ruleset => erl_files },
 #{ dirs => ["."],
    filter => "rebar.config",
    ruleset => rebar_config },
 #{ dirs => ["."],
    filter => "elvis.config",
    ruleset => elvis_config }]
```
