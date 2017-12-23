(*
 * File: ml4.ml
 *)

open Common

(* Problem 1*)
let asMonoTy1 () = mk_fun_ty bool_ty (mk_list_ty int_ty)
let asMonoTy2 () = 
  let a = fresh() in
  let b = fresh() in
  let c = fresh() in
  let d = fresh() in
    mk_fun_ty a (mk_fun_ty b (mk_fun_ty c d))
let asMonoTy3 () = 
  let a = fresh() in
  let b = fresh() in
    mk_fun_ty a (mk_list_ty (mk_pair_ty b int_ty))
let asMonoTy4 () = 
  let a = fresh() in
  let b = fresh() in
    mk_pair_ty string_ty (mk_fun_ty (mk_list_ty b) a)

(* Problem 2*)
let rec subst_fun subst m = 
  match subst with [] -> TyVar(m)
  | ((a, b)::tl) -> 
    if a = m then b
    else subst_fun tl m

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = 
  match monoTy with TyVar(m) -> subst_fun subst m
  | TyConst(s, args) ->
    let rec helper l =
      match l with [] -> []
      | (hd::tl) -> monoTy_lift_subst subst hd :: helper tl
    in TyConst(s, helper args)

(* Problem 4*)
let rec occurs x ty = 
  match ty with TyVar(v) ->
    if x = v then true
    else false
  | TyConst(s, args) ->
    let rec helper l =
      match l with [] -> false
      | (hd :: tl) -> 
        if occurs x hd then true
        else helper tl
    in helper args

(* Problem 5*)
let rec unify eqlst = 
  match eqlst with
    []->Some []
  |(s,t)::tl->(
      if s = t then unify tl else(
        match s,t with
          TyConst _, TyVar _-> unify ((t, s)::tl)
        | TyConst(s_name, s_args), TyConst(t_name, t_args) ->
          if s_name <> t_name then None
          else let rec helper l =
            match l with ([], []) -> []
            | ((shd::stl), (thd::ttl)) -> (shd, thd) :: (helper (stl, ttl))
            in unify ((helper (s_args, t_args)) @ tl)
        | TyVar(s_v), TyConst(t_name, t_args) -> 
          let sub = [(s_v, t)] in
          let rec helper l =
            match l with [] -> []
            | ((a, b) :: tl) -> (monoTy_lift_subst sub a, monoTy_lift_subst sub b) :: (helper tl)
          in let result = unify (helper tl) in
          (match result with None -> None
          | Some(phi) -> Some((s_v, monoTy_lift_subst phi t) :: phi))
        | _ -> None
      )
    )

    
(* Extra Credit *)
let equiv_types ty1 ty2 = 
  let rec unify eqlst = 
    match eqlst with
      []->Some []
    |(s,t)::tl->(
        if s = t then unify tl else(
          match s,t with
            TyConst _, TyVar _-> unify ((t, s)::tl)
          | TyConst(s_name, s_args), TyConst(t_name, t_args) ->
            if s_name <> t_name then None
            else let rec helper l =
              match l with ([], []) -> []
              | ((shd::stl), (thd::ttl)) -> (shd, thd) :: (helper (stl, ttl))
              | _ -> []
              in unify ((helper (s_args, t_args)) @ tl)
          | TyVar(s_v), TyConst(t_name, t_args) -> 
            let sub = [(s_v, t)] in
            let rec helper l =
              match l with [] -> []
              | ((a, b) :: tl) -> (monoTy_lift_subst sub a, monoTy_lift_subst sub b) :: (helper tl)
            in let result = unify (helper tl) in
            (match result with None -> None
            | Some(phi) -> Some((s_v, monoTy_lift_subst phi t) :: phi))
          | _ -> None
        )
      )
  in let result = unify [(ty1, ty2)] in
  match result with None -> false
  | Some(_) -> true
