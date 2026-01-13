(*main.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 10 2026*)

open Printf

let strmod100 inp =
  let len = String.length inp in
  if len <= 2 then (inp, 0)
  else
    let additional = int_of_string inp in
    let extra = (additional / 100) in
    (String.sub inp (len - 2) 2, extra)

let splitter line =
  let dir =
    if String.starts_with ~prefix:"R" line then 1 else 0
  in
  let num_str = String.sub line 1 (String.length line - 1) in
  let value, wraps = strmod100 num_str in
  (dir, int_of_string value, wraps)

let rec read_all acc_times acc_direc acc_wraps =
  try
    let line = read_line () in
    let (t, d, w) = splitter line in
    read_all (t :: acc_times) (d :: acc_direc) (w + acc_wraps)
  with End_of_file ->
    (List.rev acc_times, List.rev acc_direc, acc_wraps)

let () =
  let (direc, times, wrap) = read_all [] [] 0 in
  let (count, state, over) = Aofpga.Test_bench.password times direc 50 in
  printf "count = %d state = %d count2 = %d\n" (count - 1) state (over + wrap)
