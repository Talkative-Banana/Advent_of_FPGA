(*main.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 10 2026*)

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

let rec read_all acc_times acc_direc =
  try
    let line = read_line () in
    let (t, d) = splitter line in
    read_all (t :: acc_times) (d :: acc_direc)
  with End_of_file ->
    (List.rev acc_times, List.rev acc_direc)

let () =
  let (direc, times) = read_all [] [] in
  let (count, state) = Test_bench.password times direc 50 in
  printf "count = %d state = %d\n" (count - 1) state
