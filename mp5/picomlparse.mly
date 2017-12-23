/* Use the expression datatype defined in expressions.ml: */
%{
    open Common
    let andsugar l r = IfExp(l,r,ConstExp (BoolConst false))
    let orsugar l r = IfExp(l,ConstExp (BoolConst true), r)
    let ltsugar l r = BinOpAppExp(GreaterOp,r,l)
    let leqsugar l r = orsugar (ltsugar l r) (BinOpAppExp(EqOp, l, r))
    let geqsugar l r = orsugar (BinOpAppExp(GreaterOp,l,r)) (BinOpAppExp(EqOp, l, r))
    let neqsugar l r = IfExp(BinOpAppExp (EqOp,l,r), ConstExp (BoolConst false),
    		       			 ConstExp (BoolConst true))
%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW SEMI DSEMI DCOLON AT NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE UNIT
       HEAD TAIL PRINT FST SND EOF

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Common.dec> main

%%

main:
    expression DSEMI      			   { (Anon ( $1)) }
  | LET IDENT EQUALS expression	DSEMI 	           { (Let ($2,$4)) }
  | LET REC IDENT IDENT EQUALS expression DSEMI    { (LetRec ($3, $4, $6)) }

expression:
   op_exp				{ $1 }

op_exp:
  | pure_or_exp LOGICALOR and_exp	{ orsugar $1 $3 }
  | and_exp				{ $1 }

and_exp:
  | pure_and_exp LOGICALAND rel_exp	{ andsugar $1 $3 }
  | rel_exp				{ $1 }

rel_exp:
  | pure_rel_exp GT cons_exp		{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT cons_exp		{ ltsugar $1 $3 }
  | pure_rel_exp LEQ cons_exp		{ leqsugar $1 $3 }
  | pure_rel_exp GEQ cons_exp		{ geqsugar $1 $3 }
  | pure_rel_exp NEQ cons_exp		{ neqsugar $1 $3 }
  | cons_exp	     			{ $1 }

cons_exp:
  | pure_add_exp DCOLON cons_exp	{ BinOpAppExp(ConsOp,$1,$3) }
  | add_exp				{ $1 }

add_exp:
  | pure_add_exp plus_minus mult_exp	{ BinOpAppExp($2,$1,$3) }
  | mult_exp				{ $1 }

mult_exp:
  | pure_mult_exp times_div expo_exp 	{ BinOpAppExp($2,$1,$3) }
  | expo_exp	       			{ $1 }

expo_exp:
  | pure_app_raise_exp EXP expo_exp	{ BinOpAppExp (ExpoOp,$1,$3) }
  | nonop_exp	       	   		{ $1 }

nonop_exp:
    if_let_fun_try_monop_exp			{ $1 }
  | app_raise_exp			{ $1 }

app_raise_exp:
    app_exp				{ $1 }
  | monop_raise				{ $1 }
  | pure_app_exp monop_raise		{ AppExp($1,$2) }

monop_raise:
    monop RAISE nonop_exp		{ MonOpAppExp ($1,RaiseExp($3)) }
  | RAISE nonop_exp			{ RaiseExp $2 }

app_exp:
  | atomic_expression		{ $1 }
  | pure_app_exp nonapp_exp 	{ AppExp($1,$2) }

nonapp_exp:
    atomic_expression		{ $1 }
  | if_let_fun_try_monop_exp		{ $1 }


if_let_fun_try_monop_exp:
    TRY expression WITH exp_matches	{ match $4 with (x,e,ms) -> TryWithExp ($2, x,e, ms) }
  | LET REC IDENT IDENT EQUALS expression IN expression	{ LetRecInExp($3, $4, $6, $8) }
  | LET IDENT EQUALS expression IN expression		{ LetInExp($2, $4, $6) }
  | FUN IDENT ARROW expression				{ FunExp($2, $4) }
  | IF expression THEN expression ELSE expression	{ IfExp($2, $4, $6) }
  | monop if_let_fun_try_monop_exp     			{ MonOpAppExp ($1,$2) }

exp_matches:
    exp_match					{ (match $1 with (x,e) -> (x,e,[])) }
  | no_try_exp_match PIPE exp_matches		{ (match ($1,$3) with (x,e),(y,f,l) -> (x,e,((y,f)::l))) }

exp_match:
    pat ARROW expression { ($1, $3) }

no_try_exp_match:
    pat ARROW no_try_expression		{ ($1, $3) }


no_try_expression:
    no_try_op_exp			{ $1 }

no_try_op_exp:
  | pure_or_exp LOGICALOR no_try_and_exp	{ orsugar $1 $3 }
  | no_try_and_exp	   		{ $1 }

no_try_and_exp:
    pure_and_exp LOGICALAND no_try_eq_exp	{ andsugar $1 $3 }
  | no_try_eq_exp	     		{ $1 }

no_try_eq_exp:
  no_try_rel_exp     			{ $1 }

no_try_rel_exp:
  | pure_rel_exp GT no_try_cons_exp	{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS no_try_cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT no_try_cons_exp	{ ltsugar $1 $3 }
  | pure_rel_exp GEQ no_try_cons_exp	{ geqsugar $1 $3 }
  | pure_rel_exp LEQ no_try_cons_exp	{ leqsugar $1 $3 }
  | pure_rel_exp NEQ no_try_cons_exp	{ neqsugar $1 $3 }
  | no_try_cons_exp    			{ $1 }

no_try_cons_exp:
  | pure_add_exp DCOLON no_try_cons_exp { BinOpAppExp(ConsOp,$1,$3) }
  | no_try_add_exp			{ $1 }

no_try_add_exp:
  | pure_add_exp plus_minus no_try_mult_exp	{ BinOpAppExp($2,$1,$3) }
  | no_try_mult_exp				{ $1 }

no_try_mult_exp:
  | pure_mult_exp times_div no_try_expo_exp	{ BinOpAppExp(IntTimesOp,$1,$3) }
  | no_try_expo_exp				{ $1 }

no_try_expo_exp:
  | pure_app_raise_exp EXP no_try_expo_exp	{ BinOpAppExp(ExpoOp,$1,$3) }
  | no_try_nonop_exp                    	{ $1 }

no_try_nonop_exp:
    no_try_if_let_fun_monop_exp		{ $1 }
  | no_try_app_raise_expression		{ $1 }

no_try_app_raise_expression:
    no_try_app_expression		{ $1 }
  | no_try_monop_expression		{ $1 }
  | pure_app_exp no_try_monop_expression	{ $1 }

no_try_monop_expression:
  | monop RAISE no_try_app_raise_expression { MonOpAppExp($1,RaiseExp($3)) }
  | RAISE no_try_app_raise_expression  { RaiseExp($2) }

no_try_app_expression:
    atomic_expression				{ $1 } 
  | pure_app_exp no_try_nonapp_expression 	{ AppExp($1,$2) }

no_try_nonapp_expression:
    atomic_expression			{ $1 }
  | no_try_if_let_fun_monop_exp		{ $1 }

no_try_if_let_fun_monop_exp:
    IF expression THEN expression ELSE no_try_expression	{ IfExp($2,$4,$6) }
  | LET IDENT EQUALS expression IN no_try_expression		{ LetInExp($2,$4,$6) }
  | LET REC IDENT IDENT EQUALS expression IN no_try_expression	{ LetRecInExp($3,$4,$6,$8) }
  | FUN IDENT ARROW no_try_expression				{ FunExp($2, $4) }
  | monop no_try_if_let_fun_monop_exp				{ MonOpAppExp ($1,$2) }

pat:
  | UNDERSCORE	{ None }
  | INT		{ Some $1 }

pure_or_exp:
  | pure_or_exp LOGICALOR pure_and_exp		{ orsugar $1 $3 }
  | pure_and_exp   			{ $1 }

pure_and_exp:
  | pure_and_exp LOGICALAND pure_eq_exp	{ andsugar $1 $3 }
  | pure_eq_exp	     			{ $1 }

pure_eq_exp:
  pure_rel_exp	     		{ $1 }

pure_rel_exp:
  | pure_rel_exp GT pure_cons_exp	{ BinOpAppExp (GreaterOp,$1,$3) }
  | pure_rel_exp EQUALS pure_cons_exp	{ BinOpAppExp (EqOp,$1,$3) }
  | pure_rel_exp LT pure_cons_exp	{ ltsugar $1 $3 }
  | pure_rel_exp GEQ pure_cons_exp	{ geqsugar $1 $3 }
  | pure_rel_exp LEQ pure_cons_exp	{ leqsugar $1 $3 }
  | pure_rel_exp NEQ pure_cons_exp	{ neqsugar $1 $3 }
  | pure_cons_exp	     		{ $1 }

pure_cons_exp:
  | pure_add_exp DCOLON pure_cons_exp   { BinOpAppExp(ConsOp,$1,$3) }
  | pure_add_exp			{ $1 }

pure_add_exp:
  | pure_add_exp plus_minus pure_mult_exp	{ BinOpAppExp($2,$1,$3) }
  | pure_mult_exp				{ $1 }

pure_mult_exp:
  | pure_mult_exp times_div pure_expo_exp 	{ BinOpAppExp($2,$1,$3) }
  | pure_expo_exp	       			{ $1 }

pure_expo_exp:
  | pure_app_raise_exp EXP pure_expo_exp	{ BinOpAppExp (ExpoOp,$1,$3) }
  | pure_app_raise_exp           { $1 }

pure_app_raise_exp:
    pure_app_exp		{ $1 }
  | pure_monop_raise 		{ $1 }
  | pure_app_exp pure_monop_raise { AppExp($1,$2) }

pure_monop_raise:
    monop RAISE pure_app_raise_exp { MonOpAppExp($1,RaiseExp($3)) }
  | RAISE pure_app_raise_exp  { RaiseExp($2) }

pure_app_exp:
    atomic_expression			{ $1 }
  | pure_app_exp atomic_expression 	{ AppExp($1,$2) }

atomic_expression:
    constant_expression         { ConstExp $1 }
  | IDENT			{ VarExp $1 }
  | list_expression		{ $1 }
  | paren_expression            { $1 }
  | monop atomic_expression		{ MonOpAppExp ($1,$2) }

list_expression:
    LBRAC list_contents			{ $2 }
 
list_exp_end:
    RBRAC				{ ConstExp NilConst }
  | SEMI list_tail				{ $2 }

list_tail:
    RBRAC				{ ConstExp NilConst }
  | list_contents			{ $1 }

list_contents:
    expression list_exp_end	{ BinOpAppExp(ConsOp,$1,$2) }

paren_expression:
    LPAREN par_exp_end			{ $2 }

par_exp_end:
    RPAREN								{ ConstExp UnitConst }
  | expression RPAREN			{ $1 }
  | expression COMMA expression RPAREN	{ BinOpAppExp (CommaOp,$1,$3) }

constant_expression:
    INT                         { IntConst $1 }
  | TRUE			{ BoolConst true }
  | FALSE			{ BoolConst false }
  | FLOAT			{ FloatConst $1 }
  | NIL	  			{ NilConst }
  | STRING			{ StringConst $1 }
  | UNIT			{ UnitConst }


monop:
  | HEAD			{ HdOp }
  | TAIL			{ TlOp }
  | PRINT			{ PrintOp }
  | NEG				{ IntNegOp }
  | FST				{ FstOp }
  | SND				{ SndOp }

plus_minus:
    PLUS				{ IntPlusOp }
  | MINUS				{ IntMinusOp }
  | DPLUS				{ FloatPlusOp }
  | DMINUS				{ FloatMinusOp }
  | CARAT				{ ConcatOp }

times_div:
    TIMES				{ IntTimesOp }
  | DIV					{ IntDivOp }
  | MOD					{ ModOp }
  | DTIMES				{ FloatTimesOp }
  | DDIV				{ FloatDivOp }
