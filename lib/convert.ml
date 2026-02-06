open Ast

let mk_expr expr_loc expr_desc = { expr_loc; expr_desc }

let mk_atom atom_loc atom_desc = { atom_loc; atom_desc }

let mk_dcl decl_loc decl_desc = { decl_loc; decl_desc }

let rec atom (a: atom) (k: atom) =
  let app = EApp (mk_expr k.atom_loc (EAtom k), [a]) in
  mk_expr a.atom_loc app

and expr (e: expr) (k: atom) =
  match e.expr_desc with
  | EAtom a -> atom a k
  | EAssert -> e
  | ELet (p, e1, e2) ->
    (match p.ppat_desc with
     | PVar id -> expr e1 (mk_atom k.atom_loc (AFun (id, expr e2 k)))
     | _ -> failwith "Expected variable pattern")
  | EApp (f, a) ->
    mk_expr e.expr_loc (EApp (f, a @ [k]))
  | EIf (a, e1, e2) ->
    mk_expr e.expr_loc (EIf (a, expr e1 k, expr e2 k))
  | EMatch (a, l) ->
    mk_expr e.expr_loc (EMatch (a, List.map (fun (p,e) -> (p, expr e k)) l ))

let decl (d: declaration) (k: atom) =
  match d.decl_desc with
  | DFun (rf, id, params, body) ->
    let k_ppat = match k.atom_desc with
      | AId x -> x
      | _ -> assert false in
    let params = params @ [k_ppat] in
    mk_dcl d.decl_loc (DFun(rf, id, params, expr body k))

let program p k =
  List.map (fun d -> decl d k) p
