open Hardcaml
open Signal
open Circuit
open Printf
open Hardcaml.Cyclesim

let modulo100 = fun (num : Signal.t) -> mux2 (num >=:. 100) (num -:. 100) num;;

let operation = fun (times : Signal.t) (direc : Signal.t) (state : Signal.t) ->
    mux2 direc (modulo100 (state +: times)) (modulo100 ((state +:. 100) -: times));;

(*count of # of times zero occured*)
let password (ltimes: int list) (ldirec: int list) (_state: int) : (int * int) =
  let times = input "times" 32 in 
  let direc = input "direc" 1 in
  let state = input "state" 32 in
  let clk   = input "clk" 1 in
  let rst   = input "rst" 1 in
  
  let newstate = operation times direc state in
  let zero = uresize (newstate ==:. 0) 32 in

  let spec = Reg_spec.create ~clock:clk ~reset:rst () in

  let count = reg_fb spec ~enable:vdd ~width:32
  ~f:(fun count_prev -> count_prev +: zero) in

  let circ = Circuit.create_exn 
    ~name:"operation" 
    [output "newstate" newstate
    ;output "count" count] in

  let sim = create circ in
  let times_i = in_port sim "times" in
  let direc_i = in_port sim "direc" in
  let state_i = in_port sim "state" in
  let clk_i   = in_port sim "clk"   in
  let rst_i   = in_port sim "rst"   in
  let count_o = out_port sim "count" in
  let newstate_o = out_port sim "newstate" in
  
  (*Increments count here therefore decrement the counter by 1 from final result*)
  rst_i := Bits.vdd;
  clk_i := Bits.gnd; cycle sim; 
  rst_i := Bits.gnd;

  let rotate (_itimes: int) (_idirec: int) (_istate: int) : int = 
    times_i := Bits.of_int ~width:32 _itimes;
    direc_i := Bits.of_int ~width:1  _idirec;
    state_i := Bits.of_int ~width:32 _istate;

    clk_i := Bits.vdd; 
    cycle sim; 
    clk_i := Bits.gnd;
    Bits.to_int !newstate_o 
  in 

  let rec operate (itimes: int list) (idirec: int list) (istate: int) : int = 
      assert (List.compare_lengths itimes idirec = 0);
      match itimes, idirec with
      | [], [] -> istate
      | t :: ts, d:: ds -> 
        let next_state = rotate t d istate in
        operate ts ds next_state 
      | _ -> failwith "Length mismatch"
  in

  let final_state = operate ltimes ldirec _state in
  (Bits.to_int !count_o, final_state);;

let strmod100 (inp : string) : string = 
  let len = String.length inp in
  if len <= 2 then inp
  else String.sub inp (len - 2) 2

let splitter (line : string) : (int * int) =
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

let (direc, times) = read_all [] [];;
let (count, state) = password times direc 50;;
print_endline ("count = " ^ string_of_int (count - 1) ^ " state = " ^ string_of_int state);;
