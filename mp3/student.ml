open Common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([], judgment), sigma))
    | VarExp x -> 
        let gamma_x = lookup_env gamma x in
        (match gamma_x
        with None -> None
            | Some gx -> (match unify [(tau, freshInstance gx)]
            with None        -> None
                | Some sigma  -> Some(Proof([], judgment), sigma)))
    | BinOpAppExp(op, e1, e2) ->
        let tau' = binop_signature op in
        let tau1 = fresh() in
            let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
            (match e1_result with None -> None
            | Some(e1_proof, sigma1) -> 
                let e2_gamma = env_lift_subst sigma1 gamma in
                let tau2 = fresh() in
                let e2_result = gather_exp_ty_substitution e2_gamma e2 tau2 in
                (match e2_result with None -> None
                | Some(e2_proof, sigma2) -> 
                    let composed = subst_compose sigma2 sigma1 in
                    let tau_fun = mk_fun_ty tau1 (mk_fun_ty tau2 tau) in
                    let left = monoTy_lift_subst composed tau_fun in
                    let unified = unify [(left, freshInstance tau')] in
                    (match unified with None -> None
                    | Some sigma -> Some(Proof([e1_proof;e2_proof], judgment), (subst_compose sigma composed)))))
    | MonOpAppExp(monop, e1) ->
        let tau1 = fresh() in
          let result = gather_exp_ty_substitution gamma e1 tau1 in
          (match result with None -> None
          | Some(proof, sigma) ->
              let tau_fun = mk_fun_ty tau1 tau in
              let sigma_fun = monoTy_lift_subst sigma tau_fun in
              let tau' = monop_signature monop in
              let unified = unify [(sigma_fun, freshInstance tau')] in
              (match unified with None -> None
              | Some(u_sigma) -> Some(Proof([proof], judgment), subst_compost u_sigma sigma))
              )
    | IfExp(e1, e2, e3) ->
        let e1_result = gather_exp_ty_substitution gamma e1 bool_ty in
        (match e1_result with None -> None
        | Some(e1_proof, sigma1) -> 
            let e2_gamma = env_lift_subst sigma1 gamma in
            let tau2 = monoTy_lift_subst sigma1 tau in
            let e2_result = gather_exp_ty_substitution e2_gamma e2 tau2 in
            (match e2_result with None -> None
            | Some(e2_proof, sigma2) ->
                let sigma21 = subst_compose sigma2 sigma1 in
                let e3_gamma = env_lift_subst sigma21 gamma in
                let tau3 = monoTy_lift_subst sigma21 tau in
                let e3_result = gather_exp_ty_substitution e3_gamma e3 tau3 in
                (match e3_result with None -> None
                | Some(e3_proof, sigma3) -> Some(Proof([e1_proof;e2_proof;e3_proof], judgment), subst_compose sigma3 sigma21))))
    | FunExp(x, e) ->
        let tau1 = fresh() in
          let tau2 = fresh() in
          let gamma1 = ins_env gamma x (polyTy_of_monoTy tau1) in
          let result = gather_exp_ty_substitution gamma1 e tau2 in
          (match result with None -> None
          | Some(proof, sigma) ->
              let sigma_tau = monoTy_lift_subst sigma tau in
              let tau_fun = mk_fun_ty tau1 tau2 in
              let sigma_fun = monoTy_lift_subst sigma tau_fun in
              let unified = unify [(sigma_tau, sigma_fun)] in
              (match unified with None -> None
              | Some(u_sigma) -> Some(Proof([proof], judgment), subst_compose u_sigma sigma)
              )
        )
    | AppExp(e1, e2) ->
        let tau1 = fresh() in
        let tau1_fun = mk_fun_ty tau1 tau in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1_fun in
        (match e1_result with None -> None
        | Some(e1_proof, sigma1) -> 
            let e2_gamma = env_lift_subst sigma1 gamma in
            let tau2 = monoTy_lift_subst sigma1 tau1 in
            let e2_result = gather_exp_ty_substitution e2_gamma e2 tau2 in
            (match e2_result with None -> None
            | Some(e2_proof, sigma2) -> Some(Proof([e1_proof;e2_proof], judgment), subst_compose sigma2 sigma1))) 
    | RaiseExp(e) ->
        let e_result = gather_exp_ty_substitution gamma e int_ty in
        (match e_result with None -> None
        | Some(e_proof, sigma) -> Some(Proof([e_proof], judgment), sigma))
    | LetInExp(x, e1, e) -> 
        let tau1 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
        (match e1_result with None -> None
        | Some(e1_proof, sigma1) -> 
            let sigma1_gamma = env_lift_subst sigma1 gamma in
            let sigma1_tau1 = monoTy_lift_subst sigma1 tau1 in
            let sigma1_tau = monoTy_lift_subst sigma1 tau in
            let genned = gen sigma1_gamma sigma1_tau1 in
            let e_gamma = ins_env sigma1_gamma x genned in
            let e_result = gather_exp_ty_substitution e_gamma e sigma1_tau in
            (match e_result with None -> None 
            | Some(e_proof, sigma2) -> Some(Proof([e1_proof;e_proof], judgment), subst_compose sigma2 sigma1)))
    | LetRecInExp(f, x, e1, e) ->
        let tau1 = fresh() in
        let tau2 = fresh() in
        let tau_fun = mk_fun_ty tau1 tau2 in
        let f_gamma = ins_env gamma f (polyTy_of_monoTy tau_fun) in
        let e1_gamma = ins_env f_gamma x (polyTy_of_monoTy tau1) in
        let e1_result = gather_exp_ty_substitution e1_gamma e1 tau2 in
        (match e1_result with None -> None
        | Some(e1_proof, sigma1) ->
            let sigma1_gamma = env_lift_subst sigma1 gamma in
            let sigma1_tau = monoTy_lift_subst sigma1 tau in
            let sigma1_tau_fun = monoTy_lift_subst sigma1 tau_fun in
            let genned = gen sigma1_gamma sigma1_tau_fun in
            let e_gamma = ins_env sigma1_gamma f genned in
            let e_result = gather_exp_ty_substitution e_gamma e sigma1_tau in
            (match e_result with None -> None
            | Some(e_proof, sigma2) -> Some(Proof([e1_proof;e_proof], judgment), subst_compose sigma2 sigma1)))
    | TryWithExp(e, n1, e1, l) -> 
        let e_result = gather_exp_ty_substitution gamma e tau in
        (match e_result with None -> None
        | Some(e_proof, sigma) -> 
            let sigma_gamma = env_lift_subst sigma gamma in
            let sigma_tau = monoTy_lift_subst sigma tau in
            let e1_result = gather_exp_ty_substitution sigma_gamma e1 sigma_tau in
            (match e1_result with None -> None
            | Some (e1_proof, sigma1) ->
                let acc_proof = [e_proof;e1_proof] in
                let acc_sigma = subst_compose sigma1 sigma in
                let rec accfun (aproof, asigma) agamma atau l = 
                    (match l with [] -> (aproof, asigma)
                    | ((n_i, e_i) :: tl) ->
                        let i_gamma = env_lift_subst asigma agamma in
                        let i_tau = monoTy_lift_subst asigma atau in
                        let i_result = gather_exp_ty_substitution i_gamma e_i i_tau in
                        (match i_result with None -> (aproof, asigma)
                        | Some(i_proof, i_sigma) -> 
                            accfun ((aproof @ [i_proof]), (subst_compose i_sigma asigma)) i_gamma i_tau tl
                        )
                    )
                in let (t_proof, t_sigma) = accfun (acc_proof, acc_sigma) sigma_gamma sigma_tau l in
                Some(Proof(t_proof, judgment), t_sigma)
            )
        )
