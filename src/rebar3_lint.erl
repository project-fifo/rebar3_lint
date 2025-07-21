-module(rebar3_lint).

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
        {fail, [{throw, {invalid_config, Message}}]} ->
            elvis_utils:abort(Message, []);
        DefaultConfig ->
            ok = elvis_core:rock(DefaultConfig)
    end.
