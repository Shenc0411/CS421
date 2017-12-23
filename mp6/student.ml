(* File: mp6.ml *)

open Mp6common;;

let rec chk_lst lst v =
  match lst with [] -> None
    | (Some(i),ecp)::rest -> if i = v then Some(ecp) else chk_lst rest v
    | (None,ecp)::_ -> Some(ecp)

let rec app_exn_cont_to_value env ec iv =
  match ec with
    ExnContVarCPS(i) -> 
      (match lookup_exn_cont env i with None -> Failed
        | Some(eck,envlst) -> app_exn_cont_to_value envlst eck iv
      )
    | EmptyExnContCPS -> UncaughtException(iv)
    | UpdateExnContCPS(optlst, eck) ->
      (match chk_lst optlst iv with None -> app_exn_cont_to_value env eck iv
	      | Some(ecp) -> Intermediate(env,ecp)
      )

let rec app_cont_to_value env k v =
  match k with External -> Final(v)
    | ContVarCPS(i) -> (match lookup_cont env i with
	                  None -> Failed
	                | Some(ck, envlst) -> app_cont_to_value envlst ck v)
    | FnContCPS(y,e) -> Intermediate(ValueBinding(y,v)::env,e)
    | ExnMatch(ec) -> 
      (match v with IntVal iv -> app_exn_cont_to_value env ec iv
        | _ -> Failed
      )


let rec one_step_exp_cps_eval env exp_cps =
  match exp_cps with
      ConstCPS(k,c) -> app_cont_to_value env k (const_to_val c)
    | VarCPS(k,s) ->
      (match lookup_value env s with
    None -> Failed
  | Some v -> app_cont_to_value env k v)
    | MonOpAppCPS(k,m,s,ec) -> 
      (match lookup_value env s with
    None -> Failed
  | Some v ->
    (match monOpApply m v with
        Value(v') -> app_cont_to_value env k v'
      | Exn(n) -> app_exn_cont_to_value env ec n
    )
      )
    | BinOpAppCPS(k,b,s1,s2,ec) ->
      (match lookup_value env s1 with
    None -> Failed
  | Some v1 ->
    (match lookup_value env s2 with
        None -> Failed
      | Some v2 ->
        (match binOpApply b v1 v2 with
      Value(v') -> app_cont_to_value env k v'
    | Exn(n) -> app_exn_cont_to_value env ec n
        )
    )
      )
    | IfCPS(s,ec1,ec2) ->
      (match lookup_value env s with
    None -> Failed
  | Some(BoolVal(true)) -> Intermediate(env,ec1)
  | Some(BoolVal(false)) -> Intermediate(env,ec2)
  | Some(_) -> Failed
      )
    | FunCPS(k,s,i1,i2,ecp) -> app_cont_to_value env k (CPSClosureVal(s,i1,i2,ecp,env))
    | FixCPS(k,s1,s2,i1,i2,ecp) -> app_cont_to_value env k (CPSRecClosureVal(s1,s2,i1,i2,ecp,env))
    | AppCPS(k,f,x,ec) ->
      (match lookup_value env x with
    None -> Failed
  | Some v ->
    (match lookup_value env f with
        None -> Failed
      | Some(CPSClosureVal(y,k',ek,e,env')) -> Intermediate([ValueBinding(y,v);ContBinding(k',(k,env));ExnContBinding(ek,(ec,env))]@env',e)
      | Some(CPSRecClosureVal(g,y,k',ek,e,env')) -> Intermediate([ValueBinding(y,v);ValueBinding(g,CPSRecClosureVal(g,y,k',ek,e,env'));ContBinding(k',(k,env));ExnContBinding(ek,(ec,env))]@env',e)
      | _ -> Failed
    )
      )