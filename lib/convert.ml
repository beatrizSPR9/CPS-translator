open Ast

let mk_expr expr_loc expr_desc = { expr_loc; expr_desc }

let mk_atom atom_loc atom_desc = { atom_loc; atom_desc }

let mk_dcl decl_loc decl_desc = { decl_loc; decl_desc }

let rec atom (a: catom) (k: catom) =
  let app = CEApp (mk_expr k.atom_loc (CEAtom k), [a]) in
  mk_expr a.atom_loc app

and expr (e: cexpr) (k: catom) =
  match e.expr_desc with
  | CEAtom a -> atom a k
  | CEAssert -> e
  | CELet (p, e1, e2) ->
    (match p.ppat_desc with
     | CPVar id -> expr e1 (mk_atom k.atom_loc (CAFun (id, expr e2 k)))
     | _ -> failwith "Expected variable pattern")
  | CEApp (f, a) ->
    mk_expr e.expr_loc (CEApp (f, a @ [k]))
  | CEIf (a, e1, e2) ->
    mk_expr e.expr_loc (CEIf (a, expr e1 k, expr e2 k))
  | CEMatch (a, l) ->
    mk_expr e.expr_loc (CEMatch (a, List.map (fun (p,e) -> (p, expr e k)) l ))
  | CEDestruct (a, pel) -> 
    mk_expr e.expr_loc (CEDestruct (a, List.map (fun (p,e) -> (p, expr e k)) pel))

let decl (d: cdeclaration) (k: catom) =
  match d.decl_desc with
  | CDFun (rf, id, params, body) ->
    let k_ppat = match k.atom_desc with
      | CAId x -> x
      | _ -> assert false in
    let params = params @ [k_ppat] in
    mk_dcl d.decl_loc (CDFun(rf, id, params, expr body k))

let program p k =
  List.map (fun d -> decl d k) p
