(*test_bench.ml
Author: Lakshay Bansal (Talkative-Banana)
Date: Jan 10 2026*)

open Hardcaml
open Hardcaml.Cyclesim
open Bits

let password (ltimes : int list) (ldirec : int list) (_state : int)
  : (int * int * int) =

  let circ = Hdl.create_circuit () in
  let sim = create circ in

  let times_i = in_port sim "times" in
  let direc_i = in_port sim "direc" in
  let state_i = in_port sim "state" in
  let clk_i   = in_port sim "clk" in
  let rst_i   = in_port sim "rst" in

  let over_o = out_port sim "over" in
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
  (to_int !count_o, final_state, to_int !over_o)

(*outputs final state and # zeroes on the input list for all parallel states*)
let password_parallel (ltimes : int list) (ldirec : int list) (lstate : int list) (lanes : int) (lane_width : int) (workers : int)
  : ((int * int) list) =
  let circ = Hdl.create_circuit_parallel lanes lane_width workers () in
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

  let rotate itimes idirec istate workers =
    let times_val =
      Bits.concat_lsb
        (itimes
         |> List.map (fun t ->
              let bt = Bits.of_int ~width:lane_width t in
              List.init lanes (fun _ -> bt))
         |> List.flatten)
    in
    let direc_val =
      Bits.concat_lsb
        (idirec
         |> List.map (fun t ->
              let bt = Bits.of_int ~width:1 t in
              List.init lanes (fun _ -> bt))
         |> List.flatten)
    in
    let state_val = 
      Bits.concat_lsb 
        (List.map 
          (fun state -> Bits.of_int ~width:lane_width state) 
          istate) 
    in
    times_i := times_val;
    direc_i := direc_val;
    state_i := state_val;

    clk_i := vdd; cycle sim;
    clk_i := gnd;
    
    bits_to_int_list ~lanes:(lanes * workers) ~lane_width:lane_width !newstate_o
  in
  
  (*istate is a list now*)
  let rec operate itimes idirec istate workers =
    match itimes, idirec with
        | [], [] -> istate
        | t :: ts, d :: ds ->
        operate ts ds (rotate t d istate workers) workers
        | _ -> failwith "Length mismatch"
  in

  let chunkify arr w =
    let n = List.length arr in
    assert ((n mod w) = 0);
    let k = n / w in

    (* initialize k empty lists *)
    let init = List.init k (fun _ -> []) in

    let rec aux i acc = function
      | [] ->
          (* reverse each chunk to restore order *)
          List.map List.rev acc
      | x :: xs ->
          let idx = i mod k in
          let acc' =
            List.mapi
              (fun j l -> if j = idx then x :: l else l)
              acc
          in
          aux (i + 1) acc' xs
    in
    aux 0 init arr
  in
  (*list of all values*)
  let final_state = operate (chunkify ltimes workers) (chunkify ldirec workers) lstate workers in
  let cnt_lst = bits_to_int_list ~lanes:(lanes * workers) ~lane_width:lane_width !count_o in
  List.map2 (fun st cnt -> (st, cnt)) final_state cnt_lst
