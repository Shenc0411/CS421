open Common
(*open List*)

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 1: Warmup (0 Points)  ******)
let consk (x, l) k = k (x::l);;
let concatk (s1, s2) k = k (s1 ^ s2);;
let string_of_intk s k = k (string_of_int(s));;
let truncatek r k = k 1;;

(***** Problem 2: Basic CPS *****)
let diff_flipk p k = subk (1, p) (fun a -> mulk (a, p) (fun b -> mulk (2, b) k))
    

(***** Problem 3: Basic CPS *****)
let quadk (a, b, c) k = mulk (a, a) (fun t1 -> mulk (2, t1) (fun t2 -> mulk (4, b) (fun t3 -> addk (t2, t3) (fun t4 -> addk (t4, c) k))))

(***** Problem 4: Basic CPS *****)
let three_freezek (s, p) k = raise(Failure "Function not implemented yet.")

(***** Problem 5: Basic CPS *****)
let shiftk (s, q) k = 
  float_addk (q, 1.57) (fun t0 ->
  float_mulk (t0, t0) (fun t1 ->
  truncatek t1 (fun t2 ->
  string_of_intk t2 (fun t3 ->
  concatk (s, t3) (fun t4 -> 
  concatk (t4, s) k
  )))));;

(***** Problem 6a: Recursion & CPS ******)
let rec list_prod l = 
  match l with [] -> 1;
  | (x::xs) ->
      let result = list_prod xs in
          x * result;;

(***** Problem 6b: Recursion & CPS ******)
let rec list_prodk l k = 
  match l with [] -> k 1
  | (x::xs) -> 
      list_prodk xs (fun t0 ->
      mulk (x, t0) k);;

(***** Problem 7a: Recursion & CPS *****)
let rec all_positive l = 
  let rec all_positive_tail l result = 
    match l with [] -> result
    | (x::xs) -> 
        if x <= 0 then all_positive_tail xs false
        else all_positive_tail xs result
  in all_positive_tail l true;;

(***** Problem 7b: Recursion & CPS *****)
let rec all_positivek l k =
  match l with [] -> k true
  | x::xs ->
      geqk (0, x) (fun t0 ->
      if t0 then k false
      else all_positivek xs k);;

(***** Problem 8a: Recursion & CPS *****)
let rec even_count l =
  match l with [] -> 0
  | x::xs -> 
      let result = even_count xs in
          if (x mod 2) = 0 then result + 1
          else result;;

let rec even_countk l k =
  match l with [] -> k 0
  | x::xs ->
      even_countk xs (fun t0 ->
        modk (x, 2) (fun t1 ->
        eqk (t1, 0) (fun t2 ->
        if t2 then k t0
        else inck t0 k)));;

(******** CONTINUATIONS For HIGHER-ORDER FUNCTIONS ********)


let rec find_all (p,l) = raise(Failure "Function not implemented yet.")

let rec find_allk (p,l) k = raise(Failure "Function not implemented yet.")

let rec sum_all (p,l) = raise(Failure "Function not implemented yet.")

let rec sum_allk (p,l) k = raise(Failure "Function not implemented yet.")


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs =
  match fs with [] -> 0
  | x::xs -> 
      let result = list_compose xs in
          x result;;

let rec list_composek fsk k = 
  match fsk with [] -> k 0
  | x::xs ->
      list_composek xs (fun t0 ->
          x t0 k);;
