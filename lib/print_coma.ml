(* CPS --> COMA *)

open Format
open Ast

(* some useful combinators *)
let pp_newline fmt () = fprintf fmt "@\n"
let pp_newline_newline fmt () = fprintf fmt "@\n@\n"
let pp_space fmt () = fprintf fmt " "
let pp_coma fmt () = fprintf fmt ", "

let pp_paren fmt () = fprintf fmt ". "

let protect_on b f =
  if b then "(" ^^ f ^^ ")"
  else f

let curly_braces b f =
  if b then "{" ^^ f ^^ "}"
  else f

let pp_constant fmt (c: constant) =
  match c with
  | CNum n -> fprintf fmt "%d" n
  | CBool b -> fprintf fmt "%b" b

let pp_op fmt (op: op) =
  match op with
  | OPAdd -> fprintf fmt "+"
  | OPMinus -> fprintf fmt "-"
  | OPMult -> fprintf fmt "*"
  | OPDiv -> fprintf fmt "/"
  | OPEq -> fprintf fmt "="
  | OPLe -> fprintf fmt "<="

let rec pp_pattern ?(_paren=false) fmt {cppat_desc; _} =
  match cppat_desc with
  | CPWild -> () (* FIXME? *)
  | CPVar x -> fprintf fmt "%s" x.id_name (* FIXME *)
  | CPCons (_, []) -> () (* TODO *)
  | CPCons (_, args) ->
      let non_wild_args = List.filter (fun p -> match p.cppat_desc with CPWild -> false | _ -> true) args in
      fprintf fmt "@[fun %a@]@ "
        (pp_print_list ~pp_sep:pp_space pp_pattern) non_wild_args (* TODO *)

let pp_id fmt {id_name; _} = fprintf fmt "%s" id_name

let rec pp_expr fmt (e: cexpr) =
  match e.cexpr_desc with
  | CEAtom a ->
      fprintf fmt "%a" (pp_atom ~paren:true ~curly:false) a
  | CEAssert -> fprintf fmt "fail"
  | CELet (x, e1, e2) ->
      fprintf fmt "let %a =@ @[<hov 2>%a@] in@ @[%a@]"
        (pp_pattern ~_paren:false) x pp_expr e1 pp_expr e2 (* TODO *)
  | CEApp (f, al) ->
      fprintf fmt ("@[<hov 2>%a @[%a@]@]")
        pp_expr f
        (pp_print_list ~pp_sep:pp_space (pp_atom ~curly:true)) al
  | CEIf (a, e1, e2) ->
      fprintf fmt "if @[%a@] @\n (-> %a) @\n @[(%a)@]"
        (pp_atom ~paren:false ~curly:true) a pp_expr e1 pp_expr e2 (* TODO *)
  | CEDestruct (a, pel) ->
      fprintf fmt "@[destruct @[%a@]@\n@[%a@]@]"
        (pp_atom ~paren:false ~curly:true) a
        (pp_print_list ~pp_sep:pp_newline pp_ppat_cexpr) pel

and pp_atom ?(paren=false) ?(curly=false) fmt (a: catom) =
  match a.catom_desc with
  | CABinop (e1, op, e2) ->
      fprintf fmt
        (protect_on paren (curly_braces curly "@[%a %a %a@]"))
        pp_expr e1 pp_op op pp_expr e2 (* TODO *)
  | CACst c -> fprintf fmt (curly_braces curly "%a") pp_constant c
  | CAFun (x, e) ->
      fprintf fmt (protect_on true "@[fun %s -> @[<hov 2>%a@]@]")
        x.id_name pp_expr e
  | CAId x -> fprintf fmt (curly_braces curly "%s") x.id_name
  | CATuple al ->
      fprintf fmt "@[(%a)@]" (pp_print_list ~pp_sep:pp_coma pp_atom) al (* TODO *)
  | CACons (c, []) -> fprintf fmt "%s" c.id_name (* TODO *)
  | CACons (c, [a]) ->
      fprintf fmt (curly_braces curly "%s %a")
        c.id_name
        (pp_atom ~paren:false ~curly:true) a (* TODO *)
  | CACons (c, al) ->
      fprintf fmt (curly_braces curly "%s @[%a@]")
        c.id_name
        (pp_print_list ~pp_sep:pp_space (pp_atom ~curly:false)) al (* TODO *)

and pp_ppat_expr fmt (p, e) =
  fprintf fmt "@[<hov 2>(%a->@ @[%a@])@]"
    (pp_pattern ~_paren:false) p pp_expr e 

and pp_ppat_cexpr fmt (p, e) =
  match p with
  | [] -> fprintf fmt "@[<hov 2>(->@ @[%a@])@]" pp_expr e
  | _ -> fprintf fmt "@[<hov 2>(fun %a->@ @[%a@])@] "
    (pp_print_list ~pp_sep:pp_space pp_id) p
    pp_expr e (* TODO *)

let pp_rec fmt = function
  | Recursive -> fprintf fmt " rec"
  | NonRecursive -> ()

let pp_decl fmt (d: cdeclaration) =
  match d.cdecl_desc with
  | CDFun (rec_flag, id, args, e) ->
      fprintf fmt "@[<hov 2>let%a %s %a =@\n%a@]"
        pp_rec rec_flag
        id.id_name
        (pp_print_list ~pp_sep:pp_space pp_id) args
        pp_expr e

let pp_handler fmt (h: chandler) =
  fprintf fmt "@[<hov 2>handler %s %a =@\n%a@]"
    h.case_id.id_name
    (pp_print_list ~pp_sep:pp_space pp_id) h.case_args
    pp_expr h.case_body

let pp_program fmt =
  pp_print_list ~pp_sep:pp_newline_newline pp_handler fmt;
  pp_print_list ~pp_sep:pp_newline_newline pp_decl fmt
