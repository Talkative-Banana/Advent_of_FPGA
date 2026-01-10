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

(* parallel execution *)
(* 3200 bit signals, operate per 32-bit lane *)
let operation_parallel (times : Signal.t) (direc : Signal.t) (state : Signal.t) =
  let lanes = 100 in
  let lane_width = 32 in

  let per_lane =
    List.init lanes (fun i ->
      let lo = i * lane_width in
      let hi = lo + lane_width - 1 in

      let times_i = Signal.select times hi lo in
      let state_i = Signal.select state hi lo in
      let direc_i = Signal.select direc i i in

      let forward =
        modulo100 (state_i +: times_i)
      in
      let backward =
        modulo100 ((state_i +:. 100) -: times_i)
      in

      mux2 direc_i forward backward
    )
  in

  Signal.concat_lsb per_lane

let create_circuit_parallel lanes lane_width () =
  let times = input "times" (lanes * lane_width) in
  let direc = input "direc" (lanes) in
  let state = input "state" (lanes * lane_width) in
  let clk   = input "clk" 1 in
  let rst   = input "rst" 1 in

  let newstate = operation_parallel times direc state in 

  let equal_zero_per_lane =
    List.init lanes (fun i ->
      let lo = i * lane_width in
      let hi = lo + lane_width - 1 in
      let lane = Signal.select newstate hi lo in
        uresize (lane ==:. 0) lane_width
    )
    |> Signal.concat_lsb in

  let spec = Reg_spec.create ~clock:clk ~reset:rst () in

  let count =
    reg_fb spec ~enable:vdd ~width:(lanes * lane_width)
      ~f:(fun count_prev -> count_prev +: equal_zero_per_lane)
  in

  Circuit.create_exn ~name:"operation_parallel" 
    [ output "newstate" newstate
    ; output "count" count]
