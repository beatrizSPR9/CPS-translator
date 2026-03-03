open Ast

let rec pattern_to_args p =
  match p.ppat_desc with
  | PVar id -> [id]
  | PCons (_, args) -> List.concat(List.map pattern_to_args args)
  | PWild -> []

let rec atom a =
  let desc = 
    match a.atom_desc with
    | AId id -> CAId id
    | ACst c -> CACst c
    | ABinop (e1, op, e2) -> CABinop (expr e1, op, expr e2)
    | ATuple al -> CATuple (List.map atom al)
    | ACons (id, c) -> CACons (id, List.map atom c)
    | AFun (id, e) -> CAFun (id, expr e) in
  {catom_loc = a.atom_loc; catom_desc = desc;}


and expr e = 
  let desc = 
    match e.expr_desc with
    | EAtom a -> CEAtom (atom a)
    | EAssert -> CEAssert
    | ELet (x, e1, e2) ->
        CELet (pattern x, expr e1, expr e2) (* TODO *)
    | EApp (f, al) ->
        CEApp (expr f, List.map atom al) (* TODO *)
    | EIf (a, e1, e2) ->
        CEIf (atom a, expr e1, expr e2) (* TODO *)
    | EMatch (a, pel) ->
        CEDestruct (atom a, List.map (fun (p,e) -> (pattern_to_args p, expr e)) pel) in
  
  {cexpr_loc = e.expr_loc; cexpr_desc = desc;}

and pattern p =
  let desc = 
    match p.ppat_desc with
    | PVar id -> CPVar id
    | PCons (id, args) -> CPCons (id, List.map pattern args)
    | PWild -> CPWild in
  {cppat_loc = p.ppat_loc; cppat_desc = desc;}


let declaration d = 
  match d.decl_desc with
  | DFun (rec_flag, id, args, e) -> CDFun (rec_flag, id, args, expr e)