{

open Picomlparse
open Common

exception EndInput

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)
let numeric = ['0' - '9']
let lower_case = ['a' - 'z']
let alpha = ['a' - 'z' 'A' - 'Z' ]
let id_char = numeric | alpha | "'" | "_"
let hexadec = numeric | ['a' - 'f']

let open_comment = "(*"
let close_comment = "*)"
let whitespace = [' ' '\t' '\n']

rule token = parse
  | [' ' '\t'] { token lexbuf }  (* skip over whitespace *)
  | ['\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }

(* your rules go here *)
  | "~"         { NEG }
  | "+"     { PLUS  }
  | "-"     { MINUS  }
  | "*"     { TIMES  }
  | "/"     { DIV  }
  | "+."    { DPLUS  }
  | "-."    { DMINUS  }
  | "*."    { DTIMES  }
  | "/."    { DDIV  }
  | "^"     { CARAT  }
  | "<"     { LT  }
  | ">"     { GT  }
  | "<="    { LEQ  }
  | ">="    { GEQ  }
  | "="     { EQUALS  }
  | "<>"    { NEQ }
  | "|"     { PIPE  }
  | "->"    { ARROW  }
  | "::"    { DCOLON  }
  | ";"     { SEMI }
  | ";;"    { DSEMI }
  | "**"    { EXP }
  | "@"     { AT  }
  | "[]"    { NIL }
  | "let"   { LET  }
  | "rec"   { REC  }
  | "and"   { AND}
  | "in"    { IN  }
  | "if"    { IF  }
  | "then"  { THEN  }
  | "else"  { ELSE  }
  | "fun"   { FUN  }
  | "mod"   { MOD  }
  | "hd"    { HEAD }
  | "tl"    { TAIL }
  | "fst"   { FST }
  | "snd"   { SND }
  | "raise" { RAISE }
  | "try"   { TRY}
  | "with"  { WITH }
  | "not"   { NOT  }
  | "&&"    { LOGICALAND}
  | "||"    { LOGICALOR}
  | "["     { LBRAC  }
  | "]"     { RBRAC  }
  | "("     { LPAREN  }
  | ")"     { RPAREN  }
  | ","     { COMMA  }
  | "_"     { UNDERSCORE }
  | "print_string" { PRINT }

  | numeric+ as s { INT (int_of_string s) }
  | ("0x"(hexadec)+) as s { INT (int_of_string s) }
  | ((numeric+)'.'(numeric*)('e'(numeric)+)?) as s       { FLOAT (float_of_string s) }

  | "true"  { TRUE }
  | "false" { FALSE }
  | "(" whitespace* ")"    { UNIT }
  | "[" whitespace* "]"    { NIL }

  | (lower_case (id_char*)) as s     { IDENT s }

  | open_comment       { comment 1 lexbuf }

  | close_comment      { raise (Failure "unmatched closed comment") }

  | "\""    { string "" lexbuf }
  | ("//"([^'\n']*))	{ token lexbuf }

and comment count = parse
   open_comment        { comment (1 + count) lexbuf }
 | close_comment       { match count with 0 -> raise (Failure "Solution error")
                         | 1 -> token lexbuf
                         | n -> comment (n - 1) lexbuf
 }
 | eof             { raise (Failure "unmatched open comment") }
 | _                   { comment count lexbuf }

and string start_string = parse
   "\""     { STRING start_string }
 | "\\"	    { escaped_string start_string lexbuf }
 | _ as c   { string (start_string ^ (String.make 1 c)) lexbuf }

and escaped_string start_string = parse
 | "\\"   { string (start_string ^ "\\") lexbuf }
 | "\""   { string (start_string ^ "\"") lexbuf }
 | "t"    { string (start_string ^ "\t") lexbuf }
 | "n"    { string (start_string ^ "\n") lexbuf }
 | "r"    { string (start_string ^ "\r") lexbuf }
 | "b"    { string (start_string ^ "\b") lexbuf }
 | " "	  { string (start_string ^ " ") lexbuf }

 | (['0' - '1']['0' - '9']['0' - '9'] as ch) { string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
 | (['2']['0' - '4']['0' - '9'] as ch) { string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
 | (['2']['5']['0' - '5'] as ch) { string (start_string ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 

 | "\n"         { stringwhitespace start_string lexbuf }

and stringwhitespace start_string = parse
    [' ' '\t']  { stringwhitespace start_string lexbuf }
 | "\""     { STRING start_string }
 | "\\"	    { escaped_string start_string lexbuf }
 | _ as c   { string (start_string ^ (String.make 1 c)) lexbuf }

(* do not modify this function: *)
{ let lextest s = token (Lexing.from_string s)

  let get_all_tokens s =
      let b = Lexing.from_string (s^"\n") in
      let rec g () =
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }
