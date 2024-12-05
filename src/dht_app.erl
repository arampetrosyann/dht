%%%-------------------------------------------------------------------
%% @doc DHT app behaviour
%% @end
%%%-------------------------------------------------------------------

-module(dht_app).
-include("dht_config.hrl").
-export([start/2, stop/1]).

start(_Type, _Args) ->
    % @TODO set number of nodes here
    NodeCount = 20,
    Folder = ?log_folder ++ integer_to_list(NodeCount),
    file:make_dir(Folder)
% [dht_node:start(N) || N <- lists:seq(0, NodeCount - 1)]
% @TODO setup the ring here
.

stop(_State) ->
    ok.
