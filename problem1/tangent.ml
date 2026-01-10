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
  let states = List.init 100 (fun i ->  i) in
  let results = Aofpga.Test_bench.password_parallel times direc states in
  List.iteri (fun i (state, count) ->
    printf "lane %d: count=%d state=%d\n" i (count - 1) state
  ) results
