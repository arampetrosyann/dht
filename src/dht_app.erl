%%%-------------------------------------------------------------------
%% @doc dht public API
%% @end
%%%-------------------------------------------------------------------

-module(dht_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dht_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
