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

Read [`elvis_core`'s Configuration](https://github.com/inaka/elvis_core?#configuration)
README section for more information.
