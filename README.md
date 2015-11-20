rebar3_lint
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_lint, ".*", {git, "https://github.com/project-fifo/rebar3_lint.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 lint
    ===> Fetching rebar3_lint
    ===> Compiling rebar3_lint
    <Plugin Output>
