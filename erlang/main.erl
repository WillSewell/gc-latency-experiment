-module(main).
-export([start/0]).

-define(window_size, 200000).
-define(msg_count, 1000000).

message(N) ->
    list_to_binary(lists:duplicate(1024, N rem 256)).

push_message(_Channel, Worst, ?msg_count) ->
    io:format("Worst push time: ~p~n", [Worst]);
push_message(Channel, Worst, Id) ->
    {Elapsed, NewChannel} =
        timer:tc(fun() ->
                         array:set(Id rem ?window_size, message(Id), Channel)
                 end),
    push_message(NewChannel, max(Worst, Elapsed), Id + 1).

start() ->
    Channel = array:new(?window_size, {default, 0}),
    push_message(Channel, 0, 0).