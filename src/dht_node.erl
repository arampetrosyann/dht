-module(dht_node).
-export([
    start/1,
    init/1,
    to_binary/1,
    hash_to_id/1,
    integer_to_hex/1
]).

-import(lists, [map/2]).
-include("dht_config.hrl").

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_number(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
to_binary(Value) ->
    erlang:term_to_binary(Value).

integer_to_hex(Integer) ->
    Bin = integer_to_binary(Integer),
    lists:flatten([io_lib:format("~2.16B", [Byte]) || Byte <- binary:bin_to_list(Bin)]).

hash_to_integer(Hash) ->
    binary_to_integer(Hash).

hash_to_id(Hash) ->
    hash_to_integer(Hash) rem trunc(math:pow(2, ?m)).

generate_hash(Key) ->
    BinaryData = to_binary(Key),
    crypto:hash(sha, BinaryData).

% integer_to_hex, hash_to_id functions can be used after the function invocation
generate_identifier(N) ->
    Hash = generate_hash(N),
    hash_to_integer(Hash).

%% Check is the key in the range
% lookup(Key, PredecessorKey, SuccessorKey) ->
%     IsInRange = (PredecessorKey < Key) and (Key =< SuccessorKey),
%     IsLastNode = SuccessorKey =< PredecessorKey,
%     IsInLastNodeRange =
%         IsLastNode and
%             (((PredecessorKey < Key) and (Key =< math:pow(2, ?m))) or
%                 ((Key >= 0) and (Key =< SuccessorKey))),
%     IsBetween = IsInRange or IsInLastNodeRange,
%     IsBetween.

init(State) ->
    receive
        {get, Key} ->
            % @TODO Find the key here
            io:write([State, Key]);
        {put, Data} ->
            Hash = generate_hash(Data),
            Identifier = generate_identifier(Hash),

            % Remove
            io:write(Identifier),

            % @TODO Check where to store the data
            IsInRange = true,

            if
                IsInRange == true ->
                    ExistingKeys = map_get(keys, State),

                    NewState = State,

                    ItemExist = lists:member(Identifier, ExistingKeys) == true,

                    if
                        ItemExist == false ->
                            NewKeys = lists:append(ExistingKeys, [Identifier]),
                            NewState = maps:update(keys, NewKeys, State)
                    end,

                    init(NewState);
                IsInRange == false ->
                    % @TODO Search in another node
                    ok
            end
    end.

% Public
start(Number) ->
    State = #{
        id => generate_identifier(Number),
        number => Number,
        successor => undefined,
        predecessor => undefined,
        keys => []
    },

    % Create log file
    FileName = io_lib:format(?log_folder ++ "/node_~s.csv", [Number]),

    {ok, File} = file:open(FileName, [write]),
    file:close(File),
    %

    spawn(?MODULE, node, [State]).

% get(Key) ->
%     io:write(Key),
%     ok.

% put(Val) ->
%     io:write(Val),
%     ok.

log(State) ->
    Id = map_get(id, State),
    Number = map_get(number, State),
    Successor = map_get(successor, State),
    Predecessor = map_get(predecessor, State),
    % @TODO add keys into the log file
    Keys = map_get(keys, State),

    io:write(Keys),

    Hex = integer_to_hex(Id),
    SuccessorHex = integer_to_hex(Successor),
    PredecessorHex = integer_to_hex(Predecessor),

    FileName = io_lib:format(?log_folder ++ "/node_~s.csv", [Number]),
    Line = io_lib:format("node_~s,successor_~s,predecessor_~s|~s\n", [
        Hex, SuccessorHex, PredecessorHex, "123"
    ]),

    {ok, File} = file:open(FileName, [append]),
    file:write(FileName, Line),
    file:close(File),

    ok.
