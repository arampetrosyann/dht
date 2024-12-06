%%%-------------------------------------------------------------------
%% @doc DHT app behaviour
%% @end
%%%-------------------------------------------------------------------

-module(dht_app).
-include("dht_config.hrl").
-export([start/0, stop/0]).

start() ->
    % @TODO set number of nodes here
    NodeCount = 20,
    Folder = ?log_folder ++ integer_to_list(NodeCount),
    file:make_dir(Folder)
% [dht_node:start(N) || N <- lists:seq(0, NodeCount - 1)]
% @TODO setup the ring here
.

stop() ->
    ok.
