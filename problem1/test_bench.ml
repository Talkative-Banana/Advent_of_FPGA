(*test_bench.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 10 2026*)

open Hardcaml
open Hardcaml.Cyclesim
open Bits

let password (ltimes : int list) (ldirec : int list) (_state : int)
  : (int * int) =

  let circ = Hdl.create_circuit () in
  let sim = create circ in

  let times_i = in_port sim "times" in
  let direc_i = in_port sim "direc" in
  let state_i = in_port sim "state" in
  let clk_i   = in_port sim "clk" in
  let rst_i   = in_port sim "rst" in

  let count_o = out_port sim "count" in
  let newstate_o = out_port sim "newstate" in

  (* Reset *)
  rst_i := vdd;
  clk_i := gnd; cycle sim;
  rst_i := gnd;

  let rotate itimes idirec istate =
    times_i := of_int ~width:32 itimes;
    direc_i := of_int ~width:1 idirec;
    state_i := of_int ~width:32 istate;

    clk_i := vdd; cycle sim;
    clk_i := gnd;

    to_int !newstate_o
  in

  let rec operate itimes idirec istate =
    match itimes, idirec with
    | [], [] -> istate
    | t :: ts, d :: ds ->
        operate ts ds (rotate t d istate)
    | _ -> failwith "Length mismatch"
  in

  let final_state = operate ltimes ldirec _state in
  (to_int !count_o, final_state)

(*outputs final state and # zeroes on the input list for all parallel states*)
let password_parallel (ltimes : int list) (ldirec : int list) (lstate : int list)
  : ((int * int) list) =
  let lanes = 100 in
  let lane_width = 32 in

  let circ = Hdl.create_circuit_parallel lanes lane_width () in
  let sim = create circ in
  let times_i = in_port sim "times" in
  let direc_i = in_port sim "direc" in 
  let state_i = in_port sim "state" in 
  let clk_i   = in_port sim "clk" in
  let rst_i   = in_port sim "rst" in

  let count_o = out_port sim "count" in
  let newstate_o = out_port sim "newstate" in

  (* Reset *)
  rst_i := vdd;
  clk_i := gnd; cycle sim;
  rst_i := gnd;

  let bits_to_int_list ~lanes ~lane_width (b : Bits.t) =
    List.init lanes (fun i ->
      let lo = i * lane_width in
      let hi = lo + lane_width - 1 in
      Bits.to_int (Bits.select b hi lo)
    ) in

  let rotate itimes idirec istate =
    let itimes32 = Bits.of_int ~width:lane_width itimes in
    let times_val = Bits.concat_lsb (List.init lanes (fun _ -> itimes32)) in
    let idirec1 = Bits.of_int ~width:1 idirec in
    let direc_val = Bits.concat_lsb (List.init lanes (fun _ -> idirec1)) in
    let state_val = Bits.concat_lsb (List.map (fun state -> Bits.of_int ~width:lane_width state) istate) in

    times_i := times_val;
    direc_i := direc_val;
    state_i := state_val; (* 32 * 100 bits wide input *)

    clk_i := vdd; cycle sim;
    clk_i := gnd;
    
    bits_to_int_list ~lanes:lanes ~lane_width:lane_width !newstate_o
  in

  (*istate is a list now*)
  let rec operate itimes idirec istate =
    match itimes, idirec with
    | [], [] -> istate
    | t :: ts, d :: ds ->
        operate ts ds (rotate t d istate)
    | _ -> failwith "Length mismatch"
  in

  (*list of all values*)
  let final_state = operate ltimes ldirec lstate in
  let cnt_lst = bits_to_int_list ~lanes:lanes ~lane_width:lane_width !count_o in
  List.map2 (fun st cnt -> (st, cnt)) final_state cnt_lst
