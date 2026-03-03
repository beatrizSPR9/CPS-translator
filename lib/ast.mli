(** An intermediate AST for a ML-like toy language to COMA.
    All the terms are written in A-normal form to ease
    CPS-conversion later. *)

type location = Lexing.position * Lexing.position

(* Each identifier has a name and a location *)
type id = { id_name: string; id_loc: location }

type constant = CNum of int | CBool of bool

type op = OPAdd | OPMinus | OPMult | OPDiv | OPEq | OPLe

type info_p = id list 

type cpattern = {
  ppat_loc: location;
  ppat_desc: cpattern_desc;
}

and cpattern_desc =
  | CPWild                       (* _ *)
  | CPVar of id                  (* x *)
  | CPCons of id * cpattern list  (* Cons(x, xs) *)

type cexpr = {
  expr_loc: location;
  expr_desc: cexpr_desc;
}

and cexpr_desc =
  | CEAtom of catom
  | CEAssert
  | CELet of cpattern * cexpr * cexpr
  | CEApp of cexpr * catom list   (* function application *)
  | CEIf of catom * cexpr * cexpr
  | CEMatch of catom * (cpattern * cexpr) list
  | CEDestruct of catom * (info_p * cexpr) list

and catom = {
  atom_loc: location;
  atom_desc: catom_desc;
}

and catom_desc =
  | CAId of id
  | CABinop of cexpr * op * cexpr
  | CACst of constant
  | CAFun of id * cexpr
  | CATuple of catom list
  | CACons of id * catom list

type crec_flag = Recursive | NonRecursive

type cdeclaration = {
  decl_loc: location;
  decl_desc: cdeclaration_desc;
}

(* id list are the parameters and expr is the body *)
and cdeclaration_desc =
  | CDFun of crec_flag * id * id list * cexpr

type cprogram = cdeclaration list
