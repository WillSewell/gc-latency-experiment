-module(main).
-export([start/0]).

start() ->
  push_message(0, [], 0).

push_message(1000000, _, Worst) -> io:format("Worst push time: ~p~n", [Worst]);
push_message(MsgCount, _, Worst) ->
  Start = time_microseg(),
  NewStore = create_message(MsgCount),
  End = time_microseg(),
  case (End - Start) > Worst of
    true -> push_message(MsgCount + 1, NewStore, (End - Start));
    false -> push_message(MsgCount + 1, NewStore, Worst)
  end.

create_message(I) -> create_message(1024, I, []).

create_message(0, _,Bytes) -> Bytes;
create_message(Size, I, Bytes) ->
  create_message(Size - 1, I , [I | Bytes]).

time_microseg() ->
  {MS, S, US} = now(),
  (MS * 1.0e+12) + (S * 1.0e+6) + US.
