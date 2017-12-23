(* File: common.ml *)

let output_str = ref "";;

let print_string str =
  (output_str := !output_str ^ str;
  Pervasives.print_string (str));;

let print_int n = print_string ((string_of_int n) );;
let print_float x = print_string ((string_of_float x));;
let print_newline () = print_string "\n";;
let print_endline s = print_string (s^"\n");;

let print_string_list l = print_string ("[" ^ String.concat "; " l ^ "]\n ");;
(*
  let int_list l = "[" ^ String.concat "; " (List.map string_of_int l) ^ "] ";;
*)
let print_int_list l = print_string ("[" ^ String.concat "; " (List.map string_of_int l) ^ "]\n");;
let print_float_list l = print_string ("[" ^ String.concat "; " (List.map string_of_float l) ^ "]\n ");;
let print_pair_int_list (x,y) =
  print_string ("(" ^
                "[" ^ String.concat "; " (List.map string_of_int x) ^ "]" ^
                ", " ^
                "[" ^ String.concat "; " (List.map string_of_int y) ^ "]" ^
                ")\n");;

let report_float x =
   print_string "Result: ";
   print_float x;
   print_newline();;

let report_int x =
   print_string "Result: ";
   print_int x;
   print_newline();;


let inck n k = k (n + 1);;
let deck n k = k (n - 1);;
let addk (a, b) k = k (a + b);;
let subk (a, b) k = k (a - b);;
let mulk (a, b) k = k (a * b);;
let modk (a, b) k = k (a mod b);;
let float_addk (a, b) k = k (a +. b);;
let float_subk (a, b) k = k (a -. b);;
let float_mulk (a, b) k = k (a *. b);;
let geqk (a, b) k = k (a >= b);;
let leqk (a, b) k =  k (a <= b);;
let ltk (a, b) k = k (a < b);;
let gtk (a, b) k = k (a > b);;
let eqk (a, b) k = k (a = b);;
let neqk (a, b) k = k (a <> b);;
let notk b k = k (not b);;


(* For more complete testing, we will use these *)

type action =
    Inc of int
  | Dec of int
  | Add of (int * int)
  | Sub of (int * int)
  | Mul of (int * int)
  | Mod of (int * int)
  | FAdd of (float * float)
  | FSub of (float * float)
  | FMul of (float * float)
  | Geq of (int * int)
  | Leq of (int * int)
  | Gt of (int * int)
  | Lt of (int * int)
  | Eq of (int * int)
  | Neq of (int * int)
  | Not of bool

let trace = ref ([]:action list);;

let grab_trace result = 
    (let actions = !trace in (trace := []; (result, actions)))

let inck n k = (let _ = (trace := (Inc n) :: (!trace)) in k (n + 1));;
let deck n k = (let _ = (trace := (Dec n) :: (!trace)) in k (n - 1));;
let addk (a, b) k = (let _ = (trace := (Add (a,b)) :: (!trace)) in k (a + b));;
let subk (a, b) k = (let _ = (trace := (Sub (a,b)) :: (!trace)) in k (a - b));;
let mulk (a, b) k = (let _ = (trace := (Mul (a,b)) :: (!trace)) in k (a * b));;
let modk (a, b) k = (let _ = (trace := (Mod (a,b)) :: (!trace)) in k (a mod b));;
let float_addk (a, b) k = (let _ = (trace := (FAdd (a,b)) :: (!trace)) in k (a +. b));;
let float_subk (a, b) k = (let _ = (trace := (FSub (a,b)) :: (!trace)) in k (a -. b));;
let float_mulk (a, b) k = (let _ = (trace := (FMul (a,b)) :: (!trace)) in k (a *. b));;
let geqk (a, b) k = (let _ = (trace := (Geq (a,b)) :: (!trace)) in k (a >= b));;
let leqk (a, b) k = (let _ = (trace := (Geq (a,b)) :: (!trace)) in k (a <= b));;
let ltk (a, b) k = (let _ = (trace := (Geq (a,b)) :: (!trace)) in k (a < b));;
let gtk (a, b) k = (let _ = (trace := (Geq (a,b)) :: (!trace)) in k (a > b));;
let eqk (a, b) k = (let _ = (trace := (Eq (a,b)) :: (!trace)) in k (a = b));;
let neqk (a, b) k = (let _ = (trace := (Neq (a,b)) :: (!trace)) in k (a = b));;
let notk b k = (let _ = (trace := (Not b) :: (!trace)) in k (not b));;
