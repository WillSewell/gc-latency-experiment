(* This is a quick&dirty script to parse a log file produced by the
   OCaml instrumented runtime, version 4.03.0, and play with it from
   the toplevel using naive statistical functions.

   Build with
     ocamlbuild -use-ocamlfind -package batteries read_log.byte
 *)

(* The format of the produced log files is currently undocumented,
   I just looked at it and guessed that the two numbers in most lines
   are the starting time and ending time of the recorded event (in
   nanoseconds).

   In practice not all lines correspond to time intervals in this way,
   some seem record integer variables instead. They will put
   a (usually small) integer in place of the second number. I'm not
   sure what in the format indicates which kind of data a given line
   is, so I use the dumb heuristic that (start <= stop) must hold for
   time intervals, and discard all lines not matching this condition.

   There is more knowledge in the file formats embedded in the tools
   included in the OCaml distribution, namely
     tools/ocaml-instr-report
     tools/ocaml-instr-graph

   If you wanted to write your own script or extend this one into
   a reusable library, you should go read the code of these tools to
   learn more about the log format.
 *)

let read_log_line line =
  Scanf.sscanf line "@@ %19d %19d %s@\n"
               (fun start stop name ->
                if start > stop then None
                else Some (stop - start, name))

let events_of_file file =
  let ic = open_in file in
  (* ignore header file *) ignore (input_line ic);
  let events =
    let read_event () = read_log_line (input_line ic) in
    let (evs, _exn) = BatList.unfold_exc read_event in
    BatList.filter_map (fun ev_option -> ev_option) evs in
  close_in ic;
  let cmp (t1, _) (t2, _) = compare t1 (t2 : int) in
  List.sort cmp events |> Array.of_list

let dispatch events =
  BatArray.filter (fun (_time, name) -> name = "dispatch") events

let representatives n times =
  let len = Array.length times in
  let repr = Array.init n (fun i -> times.(i * len / n)) in
  (times.(0), repr, times.(len - 1))

let histogram n times =
  let len = Array.length times in
  let low, high = fst times.(0), fst times.(len - 1) in
  let bins = Array.make n [] in
  let bin ((time, _) as ev) =
    let i = (n - 1) * (time - low) / (high - low) in
    bins.(i) <- ev :: bins.(i);
  in
  Array.iter bin times;
  Array.map List.rev bins

type display_bin = { count: int; min: int; max: int }
let display_histogram histo =
  let display bin =
    if bin = [] then None
    else begin
        let times = List.map fst bin in
        let count = List.length times in
        let min, max = BatList.min_max times in
        Some { count; min; max }
      end in
  Array.map display histo
