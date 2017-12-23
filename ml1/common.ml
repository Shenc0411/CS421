(* This is fragile and assumes Solution is run second, and that each
Solution function uses print_string at most once, and that the
students don't use print_string when the problem doesn't call for
it. *)

let output = ref "";;
let last_string_length = ref (-1);;

let print_string str = 
  output := !output ^ str;
  last_string_length := String.length str;
  Pervasives.print_string (str)

let print_newline () = print_string "\n";;

let print_endline s = print_string (s^"\n");;

