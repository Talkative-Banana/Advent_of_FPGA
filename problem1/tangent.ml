(*stress_test.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 11 2026*)

(*
Iteration #1

Part 1: Divide the input among workers
  Assuming # of workers n
  length of input for primary worker (l / n)
  length of input for non primary chunk (l / n) for n > 0)
Part 2:
  Run them all in parallel
  Non primary workers will compute and store their results for all staring states [0 ... 100] parallely
Part 3:
  Once primary worker is done with its work, it can use the results of other workers to finish the job.

To simplify:
  Let the primary worker also compute all of the values
  Decrease the size of lane_width?
*)

open Printf

let strmod100 inp =
  let len = String.length inp in
  if len <= 2 then inp
  else String.sub inp (len - 2) 2

let splitter line =
  if String.starts_with ~prefix:"R" line then
    (1, int_of_string (strmod100 (String.sub line 1 (String.length line - 1))))
  else
    (0, int_of_string (strmod100 (String.sub line 1 (String.length line - 1))))

let rec read_all_lines lines acc_times acc_direc =
  match lines with
  | [] -> (List.rev acc_times, List.rev acc_direc)
  | line :: rest ->
      let (t, d) = splitter line in
      read_all_lines rest (t :: acc_times) (d :: acc_direc)

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let () =
  let filename = Sys.argv.(1) in
  let contents = read_file filename in
  let lines =
    String.split_on_char '\n' contents
    |> List.filter (fun s -> String.trim s <> "") in
  let (direc, times) = read_all_lines lines [] [] in
  let lanes = 100 in
  let lane_width = 16 in
  let workers = 3 in
  let intitial_state = 50 in
  let states = List.init (lanes * workers) (fun i ->  (i mod lanes)) in
  let n = List.length times in
  let normalized_times = List.append (List.init (workers - (n mod workers)) (fun _ -> 0)) (times) in
  let normalized_direc = List.append (List.init (workers - (n mod workers)) (fun _ -> 0)) (direc) in
  let results = Aofpga.Test_bench.password_parallel normalized_times normalized_direc states lanes lane_width workers in
  let rec read_entry arr worker state count = 
    if worker >= workers then 
      (state, count)
    else 
    let idx = worker * lanes + state in
    let (new_lane, new_count) = List.nth arr idx in
    read_entry arr (worker + 1) new_lane (count + new_count)
  in
  List.iteri (fun i (state, count) ->
  if (i mod lanes == 0) then printf "-------------------------------------------\n";
  printf "worker %d: lane %d: count = %d state = %d\n" (i / lanes) (i mod lanes) (count - 1) state;
  ) results;
  printf "-------------------------------------------\n";
  let (state, count) = read_entry results 0 intitial_state 0 in
  printf "Final Result: count = %d state = %d\n" (count - workers) (state mod lanes) 
