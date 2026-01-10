(*hdl.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 10 2026*)

open Hardcaml
open Signal

let modulo100 (num : Signal.t) =
  mux2 (num >=:. 100) (num -:. 100) num

let operation (times : Signal.t) (direc : Signal.t) (state : Signal.t) =
  mux2 direc
    (modulo100 (state +: times))
    (modulo100 ((state +:. 100) -: times))

let create_circuit () =
  let times = input "times" 32 in
  let direc = input "direc" 1 in
  let state = input "state" 32 in
  let clk   = input "clk" 1 in
  let rst   = input "rst" 1 in

  let newstate = operation times direc state in
  let zero = uresize (newstate ==:. 0) 32 in

  let spec = Reg_spec.create ~clock:clk ~reset:rst () in

  let count =
    reg_fb spec ~enable:vdd ~width:32
      ~f:(fun count_prev -> count_prev +: zero)
  in

  Circuit.create_exn
    ~name:"operation"
    [ output "newstate" newstate
    ; output "count" count]
