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
