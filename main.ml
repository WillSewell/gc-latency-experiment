open Batteries

module IMap = Map.Make(Int)

type msg = string

let message n = String.make 1024 (Char.chr (n mod 256))

let worst = ref 0.

let window_size = 200_000
let msg_count = 1_000_000

let time f =
  let before = Unix.gettimeofday () in
  let result = f () in
  let after = Unix.gettimeofday () in
  worst := max !worst (after -. before);
  result

let push_msg chan high_id = time @@ fun () ->
  let low_id = high_id - window_size in
  let inserted = IMap.add high_id (message high_id) chan in
  if low_id < 0 then inserted
  else IMap.remove low_id inserted

let () =
  begin
    Seq.init 1_000_000 (fun i -> i)
    |> Seq.fold_left push_msg IMap.empty
    |> ignore
  end;
  Printf.printf "Worst pause: %.2E\n" !worst
