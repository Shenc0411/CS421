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
let rec even_count_fr l = raise(Failure "Function not implemented yet.")

(* Problem 2 *)
let rec pair_sums l = raise(Failure "Function not implemented yet.")

(* Problem 3 *)
let rec remove_even list = raise(Failure "Function not implemented yet.")

(* Problem 4 *)
let rec sift p l = raise(Failure "Function not implemented yet.")

(* Problem 5 *)
let rec apply_even_odd l f g = raise(Failure "Function not implemented yet.")

(* Problem 6 *)
let rec rle lst = raise(Failure "Function not implemented yet.")

(******************
 * Tail Recursion *
 ******************)

(* Problem 7 *)
let rec even_count_tr l = raise(Failure "Function not implemented yet.")

(* Problem 8 *)
let rec count_element l m = raise(Failure "Function not implemented yet.")

(* Problem 9 *)
let rec all_nonneg list = raise(Failure "Function not implemented yet.")

(* Problem 10 *)
let split_sum l f = raise(Failure "Function not implemented yet.")

(* Problem 11 *)
let rec max_index l = raise(Failure "Function not implemented yet.")

(* Problem 12 *)
let rec concat s list = raise(Failure "Function not implemented yet.")

(**************************
 * Higher Order Functions *
 **************************)

(* Problem 13 *)
let even_count_fr_base = 1337 (* You may need to change this *)
let even_count_fr_rec x rec_val = raise(Failure "Function not implemented yet.")

(* Problem 14 *)
let pair_sums_map_arg p = raise(Failure "Function not implemented yet.")

(* Problem 15 *)
let remove_even_base = [2; 4; 8; 16] (* You may need to change this *)
let remove_even_rec n r = raise(Failure "Function not implemented yet.")

(* Problem 16 *)
let sift_base = ([],[]) (* You may need to change this *)
let sift_rec p x (tl, fl) = raise(Failure "Function not implemented yet.")

(* Problem 17 *)
let even_count_tr_start = 1337 (* You may need to change this *)
let even_count_tr_step acc_val c = raise(Failure "Function not implemented yet.")

(* Problem 18 *)
let count_element_start = 1337 (* You may need to change this *)
let count_element_step m = raise(Failure "Function not implemented yet.")


(* Problem 19 *)
let all_nonneg_start = false;; (* You may need to change this *)
let all_nonneg_step r x = raise(Failure "Function not implemented yet.")

(* Problem 20 *)
let split_sum_start = (1442,1221) (* You may need to change this *)
let split_sum_step f = raise(Failure "Function not implemented yet.")

(* Problem 21 *)
let app_all_with fs b l = raise(Failure "Function not implemented yet.")

(* Problem 22 *)
let exists_between_start = true (* You may need to change this *)
let exists_between_step m n b x = raise(Failure "Function not implemented yet.")

let rev_append_base l = raise(Failure "Function not implemented yet.")
let rev_append_rec x = raise(Failure "Function not implemented yet.")
