-module(rebar3_lint).

%% The rebar3 plugin architecture has a poorly defined "behaviour"
-elvis([{elvis_style, consistent_ok_error_spec, disable}]).

-export([init/1]).
%% for eating our own dogfood
-export([main/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_lint_prv:init(State),
    {ok, State1}.

%% @private
main([]) ->
    ok = application:load(elvis_core),
    case elvis_config:config() of
        {error, Message} ->
            elvis_utils:abort(Message, []);
        ElvisConfig ->
            case elvis_core:rock(ElvisConfig) of
                ok ->
                    ok;
                {errors, _} ->
                    io:format(standard_error, "Elvis: linting failed\n", []),
                    erlang:halt(1);
                {warnings, _} ->
                    ok
            end
    end.
