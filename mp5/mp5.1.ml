open Common;;

let const_to_val c = 
  match c with
    IntConst i -> IntVal i
    | BoolConst b -> BoolVal b
    | FloatConst f -> FloatVal f
    | StringConst s -> StringVal s
    | NilConst -> ListVal []
    | UnitConst -> UnitVal

let monOpApply op v = 
  match op with
    HdOp -> (match v with ListVal l -> List.hd l)
    | TlOp -> (match v with ListVal l -> ListVal(List.tl l))
    | FstOp -> (match v with PairVal (a, b) -> a)
    | SndOp -> (match v with PairVal (a, b) -> b)
    | IntNegOp -> (match v with  IntVal i -> IntVal (-i))
    | PrintOp -> (match v with StringVal s -> print_string s; UnitVal)

let binOpApply binop (v1, v2) = 
  match binop with
    IntPlusOp -> let IntVal i1 = v1 in let IntVal i2 = v2 in IntVal(i1 + i2)
    | IntMinusOp -> let IntVal i1 = v1 in let IntVal i2 = v2 in IntVal(i1 - i2)
    | IntTimesOp -> let IntVal i1 = v1 in let IntVal i2 = v2 in IntVal(i1 * i2)
    | IntDivOp -> let IntVal i1 = v1 in let IntVal i2 = v2 in IntVal(i1 / i2)
    | ModOp -> let IntVal i1 = v1 in let IntVal i2 = v2 in IntVal(i1 mod i2)
    | FloatDivOp -> let FloatVal i1 = v1 in let FloatVal i2 = v2 in FloatVal(i1 /. i2)
    | FloatMinusOp -> let FloatVal i1 = v1 in let FloatVal i2 = v2 in FloatVal(i1 -. i2)
    | FloatPlusOp -> let FloatVal i1 = v1 in let FloatVal i2 = v2 in FloatVal(i1 +. i2)
    | FloatTimesOp -> let FloatVal i1 = v1 in let FloatVal i2 = v2 in FloatVal(i1 *. i2)
    | ExpoOp -> let FloatVal i1 = v1 in let FloatVal i2 = v2 in FloatVal(i1 ** i2)
    | ConcatOp -> let StringVal s1 = v1 in let StringVal s2 = v2 in StringVal(s1 ^ s2)
    | ConsOp -> let ListVal l = v2 in ListVal (v1 :: l)
    | CommaOp -> PairVal(v1, v2)
    | EqOp -> BoolVal(v1 = v2)
    | GreaterOp -> BoolVal(v1 > v2)

let rec eval_exp (exp, m) = match exp with
  ConstExp t -> const_to_val t
  | VarExp x ->
    let v = lookup_mem m x in (match v with 
      RecVarVal(g, y, e, m') -> Closure(y, e, (ins_mem m' g (RecVarVal(g, y, e, m'))))
      | _ -> v)
  | MonOpAppExp (op, e) -> monOpApply op (eval_exp (e, m))
  | BinOpAppExp (op, e1, e2) -> binOpApply op ((eval_exp (e1, m)), (eval_exp (e2, m)))
  | IfExp (e1, e2, e3) ->
    let v1 = eval_exp (e1, m) in
      if v1 = BoolVal true then eval_exp (e2, m)
      else eval_exp(e3, m)
  | LetInExp (x, e1, e2) -> 
    let v1 = eval_exp (e1, m) in eval_exp (e2, (ins_mem m x v1))
  | FunExp (x, e) -> Closure(x, e, m)
  | AppExp (e1, e2) -> 
    let Closure(x, e', m') = eval_exp (e1, m) in
    let v' = eval_exp (e2, m) in eval_exp (e', (ins_mem m' x v'))
  | LetRecInExp (f, x, e1, e2) -> eval_exp(e2, (ins_mem m f (RecVarVal(f, x, e1, m))))
  | _ -> raise (Failure "Not implemented yet.")

let eval_dec (dec, m) = match dec with
  Anon e -> ((None, eval_exp(e, m)), m)
  | Let (x, e) -> let v = eval_exp(e, m) in ((Some x, v), (ins_env m x v))
  | LetRec (f, x, e) -> 
    let rv = RecVarVal(f, x, e, m) in ((Some f, rv), (ins_mem m f rv))
