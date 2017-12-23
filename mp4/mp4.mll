{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 

(* your rules go here *)
  | '~'             { NEG }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | "+."            { DPLUS }
  | "-."            { DMINUS }
  | "*."            { DTIMES }
  | "/."            { DDIV }
  | '^'             { CARAT }
  | '<'             { LT }
  | '>'             { GT }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | '='             { EQUALS }
  | "<>"            { NEQ }
  | '|'             { PIPE }
  | "->"            { ARROW }
  | ';'             { SEMI }
  | ";;"            { DSEMI }
  | "::"            { DCOLON }
  | '@'             { AT }
  | "[]"            { NIL }
  | "let"           { LET }
  | "rec"           { REC }
  | "and"           { AND }
  | "end"           { END }
  | "in"            { IN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "fun"           { FUN }
  | "mod"           { MOD }
  | "raise"         { RAISE }
  | "try"           { TRY }
  | "with"          { WITH }
  | "not"           { NOT }
  | "&&"            { LOGICALAND }
  | "||"            { LOGICALOR }
  | '['             { LBRAC }
  | ']'             { RBRAC }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '_'             { UNDERSCORE }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "()"            { UNIT }
  | numeric+ as int {INT (int_of_string int) }
  | "0b" ['0' '1']+ as bin {INT (int_of_string bin)}
  | "0x" ['0' - '9' 'a' - 'f']+ as hex {INT (int_of_string hex)}
  | numeric+ '.' numeric* as float {FLOAT (float_of_string float)}
  | numeric+ '.' numeric* 'e' numeric+ as snotation {FLOAT (float_of_string snotation)} 
  | lowercase ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '\'']* as ident {IDENT (ident)}
  | "//" { lineComment lexbuf}
  | "(*" { blockComment 0 lexbuf }
  | "*)" { raise (Failure "unmatched closed comment") }
  | "\"" { STRING(lex_string lexbuf)}
  

and lineComment = parse
  | '\n'      {token lexbuf}
  | eof       {EOF}
  | _         {lineComment lexbuf}

and blockComment n = parse
  | "*)" { if n > 0 then blockComment (n - 1) lexbuf else token lexbuf }
  | "(*" { blockComment (n + 1) lexbuf }
  | eof { raise (Failure "unmatched open comment") }
  | _ { blockComment n lexbuf }

and lex_string = parse
  | "\\\\"                                        { "\\" ^ (lex_string lexbuf) }
  | "\\'"                                         { "\'" ^ (lex_string lexbuf) }
  | "\\\""                                        { "\"" ^ (lex_string lexbuf) }
  | "\\t"                                         { "\t" ^ (lex_string lexbuf) }
  | "\\n"                                         { "\n" ^ (lex_string lexbuf) }
  | "\\r"                                         { "\r" ^ (lex_string lexbuf) }
  | "\\b"                                         { "\b" ^ (lex_string lexbuf) }
  | "\\ "                                         { " " ^ (lex_string lexbuf) }
  | "\""                                          { "" }
  | "\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] as value) { (String.make 1 (char_of_int(int_of_string value))) ^ (lex_string lexbuf) }
  | [' ' '!' '#'-'~'] as s                        { (String.make 1 s) ^ (lex_string lexbuf) }
  | "\\\n" [' ' '\t']*                            { lex_string lexbuf }


{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

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

