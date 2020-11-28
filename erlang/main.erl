-module(main).
-export([start/0]).

start() ->
    push_message(0, [], 0).

push_message(1000000, _, Worst) -> io:format("Worst push time: ~p~n", [Worst]);
push_message(MsgCount, Store, Worst) ->
    Start = time_microseg(),
    NewStore = create_message(MsgCount),
    ElapsedMs = time_microseg() - Start,
    case {ElapsedMs > Worst, length(Store)} of
        {true, 200000} -> 
            [_|TStore] = Store,
            push_message(MsgCount + 1, TStore ++ NewStore, ElapsedMs);
        {true, _} ->
            push_message(MsgCount + 1, [NewStore|Store], ElapsedMs);
        {false, _} ->
            push_message(MsgCount + 1, Store, Worst)
    end.

create_message(I) -> create_message(1024, I, []).

create_message(0, _, Bytes) -> Bytes;
create_message(Size, I, Bytes) ->
    create_message(Size - 1, I , [I|Bytes]).

time_microseg() ->
    erlang:system_time('micro_seconds').
