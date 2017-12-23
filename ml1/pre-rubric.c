(*
 * grader for mp1
 * This file will be preprocessed to generate the actual OCaml file.
 *)

#define TEST0ARG(WEIGHT, FNAME)\
    #FNAME, mp1test WEIGHT (ss_pair0 Solution.FNAME Student.FNAME)
#define TEST1ARG(WEIGHT, FNAME, ARG1)\
    #FNAME^" "^#ARG1, mp1test WEIGHT (ss_pair1 Solution.FNAME Student.FNAME ARG1)
#define TEST2ARG(WEIGHT, FNAME, ARG1, ARG2)\
    #FNAME^" "^#ARG1^" "^#ARG2, mp1test WEIGHT (ss_pair2 Solution.FNAME Student.FNAME ARG1 ARG2)
#define TEST3ARG(WEIGHT, FNAME, ARG1, ARG2, ARG3)\
    #FNAME^" "^#ARG1^" "^#ARG2^" "^#ARG3, mp1test WEIGHT (ss_pair3 Solution.FNAME Student.FNAME ARG1 ARG2 ARG3)
#define TEST4ARG(WEIGHT, FNAME, ARG1, ARG2, ARG3, ARG4)\
    #FNAME^" "^#ARG1^" "^#ARG2^" "^#ARG3^" "^#ARG4, mp1test WEIGHT (ss_pair4 Solution.FNAME Student.FNAME ARG1 ARG2 ARG3 ARG4)
#define TEST5ARG(WEIGHT, FNAME, ARG1, ARG2, ARG3, ARG4, ARG5)\
    #FNAME^" "^#ARG1^" "^#ARG2^" "^#ARG3^" "^#ARG4^" "^#ARG5, mp1test WEIGHT (ss_pair5 Solution.FNAME Student.FNAME ARG1 ARG2 ARG3 ARG4 ARG5)

let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2017 ML1"

open Grader
open Test
open Common

(*
 * use a timeout of 4 seconds
 *)
let outputOk () =
  let n = !last_string_length in   
  if n < 0 then true else
  try (
  let len = String.length !output
  in let half1 = String.sub !output 0 (len - n)
  and half2 = String.sub !output (len - n) n
     in half1=half2
  ) with e -> false

(*  let len = List.length !output
  in if (len mod 2) != 0 then false else
    let rec aux slist half1 half2 idx =
      if idx = (len / 2) then half1 = half2
      else match slist with 
           |  x::y -> aux y (half1@[x]) y (idx + 1)
    in aux !output [] [] 0
*)


let isEq i j =
    (i = j) &&
    (let res = outputOk() in output := ""; last_string_length := -1; res)

let mp1test weight pair = compare isEq 4 weight pair

#include "tests"

let _ = Main.main rubric extra_rubric rubric_title rubric_version
