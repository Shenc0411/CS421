(* CS421 - Fall 2016
 * ML2
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

(*************************
 * Patterns of Recursion *
 *************************)

(*********************
 * Forward Recursion *
 *********************)

(* Problem 1 *)
let rec even_count_fr l = 
  match l with
  [] -> 0
  | (x::xs) -> 
    let result = even_count_fr xs in
      if (x mod 2) = 0 then 1 + result
      else result;;

(* Problem 2 *)
let rec pair_sums l = 
  match l with
  [] -> []
  | ((a,b)::ls) ->
    let result = pair_sums ls in
    ((a+b)::result);;

(* Problem 3 *)
let rec remove_even list = 
  match list with
  [] -> []
  | (x::xs) ->
    let result = remove_even xs in
    if (x mod 2) = 0 then result
    else (x::result);;

(* Problem 4 *)
let rec sift p l = 
  match l with
  [] -> ([],[])
  | (x::xs) ->
    let (tl,fl) = (sift p xs) in
      if(p x) then ((x::tl), fl)
      else (tl,(x::fl));;

(* Problem 5 *)
let rec apply_even_odd l f g = 
  match l with
  [] -> []
  | (x::xs) ->
    match xs with
      [] -> [f x]
      | (t::ts) -> ((f x)::(g t)::apply_even_odd ts f g);;
      

(* Problem 6 *)
let rec rle lst = 
  match lst with
  [] -> []
  | (x::xs) ->
    let result = rle xs in
      match result with
      [] -> [(x,1)]
      | ((a,b)::rs) ->
        if x = a then ((a,b+1)::rs)
        else ((x,1)::(a,b)::rs);;

(******************
 * Tail Recursion *
 ******************)

(* Problem 7 *)
let rec even_count_tr l = 
  let rec ectr l acc = 
    match l with
    [] -> acc
    | (x::xs) ->
      if (x mod 2) = 0 then (ectr xs (acc + 1))
      else (ectr xs acc)
  in ectr l 0;;

(* Problem 8 *)
let rec count_element l m = 
  let rec ce l m acc = 
    match l with
    [] -> acc
    | (x::xs) ->
      if x = m then (ce xs m (acc + 1))
      else (ce xs m acc)
  in ce l m 0;;

(* Problem 9 *)
let rec all_nonneg list = 
  let rec an list result = 
    match list with
    [] -> result
    | (x::xs) ->
       if x >= 0 then (an xs result)
       else (an xs false)
  in an list true;;
(* Problem 10 *)
let split_sum l f = 
  let rec ss l f tacc facc = 
    match l with 
    [] -> (tacc, facc)
    | (x::xs) ->
      if f x then (ss xs f (tacc + x) facc)
      else (ss xs f tacc (facc + x))
  in (ss l f 0 0);;

(* Problem 11 *)
let rec max_index l = 
  let rec mi l max_val max_list idx = 
    match l with
    [] -> max_list
    | (x::xs) ->
        if x > max_val then (mi xs x [idx] (idx + 1))
        else if x = max_val then (mi xs max_val (idx::max_list) (idx+1))
        else (mi xs max_val max_list (idx + 1))
  in match l with
  [] -> []
  | (x::xs) -> (mi xs x [0] 1);;

(* Problem 12 *)
let rec concat s list = 
  let rec cc s list result= 
    match list with
    [] -> result
    | (x::xs) -> cc s xs (result ^ s ^ x)
  in match list with
  [] -> ""
  | (x::xs) -> cc s xs x;;
(**************************
 * Higher Order Functions *
 **************************)

(* Problem 13 *)
let even_count_fr_base = 0 (* You may need to change this *)
let even_count_fr_rec x rec_val = 
  if (x mod 2) = 0 then rec_val + 1
  else rec_val;;

(* Problem 14 *)
let pair_sums_map_arg p = 
  let (a,b) = p in a + b;;

(* Problem 15 *)
let remove_even_base = [] (* You may need to change this *)
let remove_even_rec n r = 
  if (n mod 2) = 1 then (n::r)
  else r;;

(* Problem 16 *)
let sift_base = ([],[]) (* You may need to change this *)
let sift_rec p x (tl, fl) = 
  if p x then (x::tl, fl)
  else (tl, x::fl);;

(* Problem 17 *)
let even_count_tr_start = 0 (* You may need to change this *)
let even_count_tr_step x rec_val = 
  if (rec_val mod 2) = 0 then x + 1
  else x;;

(* Problem 18 *)
let count_element_start = 0 (* You may need to change this *)
let count_element_step m = 
  fun acc -> (fun x ->
    if x = m then acc + 1
    else acc);;


(* Problem 19 *)
let all_nonneg_start = true;; (* You may need to change this *)
let all_nonneg_step r x = 
  if x < 0 then false
  else r;;

(* Problem 20 *)
let split_sum_start = (0, 0) (* You may need to change this *)
let split_sum_step f = 
  fun (tacc, facc) -> fun x ->
    if f x then (tacc + x, facc)
    else (tacc, facc + x);;

(* Problem 21 *)
let app_all_with fs b l = 
  List.map (fun f -> List.map (f b) l) fs

(* Problem 22 *)
let exists_between_start = false (* You may need to change this *)
let exists_between_step m n b x =
  if x >= m && x <= n then true
  else b;;

let rev_append_base l = l;;
let rev_append_rec x = 
  fun f -> fun l -> f(x::l);;
