open Batteries

module IMap = Map.Make(Int)

type msg = { id: int; content: string }

let message n = { id = n; content = String.make 1024 (Char.chr (n mod 256)) }

let worst = ref 0.

let push_msg (count, map) msg =
  let before = Unix.gettimeofday () in
  let inserted = IMap.add msg.id msg.content map in
  let count = count + 1 in
  let result =
    if 200_000 < count
    then (count - 1, snd (IMap.pop_min_binding inserted))
    else (count, inserted) in
  let after = Unix.gettimeofday () in
  worst := max !worst (after -. before);
  result

let () =
  begin
    Seq.init 1_000_000 message
    |> Seq.fold_left push_msg (0, IMap.empty)
    |> ignore
  end;
  Printf.printf "%.2E\n" !worst
