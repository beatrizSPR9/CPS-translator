%{
  open Ast

  let mk_id id_name id_loc = { id_name; id_loc }
  let mk_expr expr_loc expr_desc = { expr_loc; expr_desc }
  let mk_atom atom_loc atom_desc = { atom_loc; atom_desc }
  let mk_decl decl_loc decl_desc = { decl_loc; decl_desc }
  let mk_ppat ppat_loc ppat_desc = { ppat_loc; ppat_desc }
  let mk_nil atom_loc =
    let nil_id = mk_id "Nil" atom_loc in
    mk_atom atom_loc (ACons (nil_id, []))
  let mk_cons atom_loc x xs =
    let cons_id = mk_id "Cons" atom_loc in
    mk_atom atom_loc (ACons (cons_id, [x; xs]))
%}

%token <string> IDENT
%token <string> UIDENT
%token <Ast.constant> CONSTANT
%token ADD MINUS MULT DIV
%token COLONCOLON LSQUARE RSQUARE
%token LE
%token PWILD
%token LET IF THEN ELSE MATCH WITH LARROW
%token BEGIN END
%token LPAR RPAR EQUAL IN COMMA PIPE SEMI
%token FUN REC
%token ASSERT
%token EOF

/* priorities and associativities */
%nonassoc IN ELSE
%right LARROW
%right COLONCOLON
%nonassoc prec_constr_appl
%left ADD MINUS
%left MULT DIV
%left EQUAL LE

%left PIPE

%start program
%type <Ast.expr> expr
%type <Ast.expr_desc> expr_desc
%type <Ast.program> program

%%

program:
| dl = list(decl) EOF { dl }
;

decl:
| LET r = rec_flag f = lident args = lident* EQUAL e = expr
    { mk_decl ($startpos, $endpos) (DFun (r, f, args, e)) }
;

lident:
| id = IDENT { mk_id id ($startpos, $endpos) }

uident:
| id = UIDENT { mk_id id ($startpos, $endpos) }

rec_flag:
| (* empty *) { NonRecursive }
| REC         { Recursive }
;

expr:
| e = expr_desc      { mk_expr ($startpos, $endpos) e }
(* | LPAR e = expr RPAR { e } *)
| BEGIN e = expr END { e }
;

expr_desc:
| a = atom
    { EAtom a }
| ASSERT c = CONSTANT
    { match c with
      | CBool false -> EAssert
      | _ -> assert false }
| LET x = mk_pat EQUAL e1 = expr IN e2 = expr
    { ELet (x, e1, e2) }
| f = lident args = nonempty_list(atom_arg)
    { let f_atom = mk_atom f.id_loc (AId f) in
      let f_expr = mk_expr f.id_loc (EAtom f_atom) in
      EApp (f_expr, args) }
| IF a = atom THEN e1 = expr ELSE e2 = expr
    { EIf (a, e1, e2) }
| MATCH a = atom WITH pe = match_cases(expr)
    { EMatch (a, pe) }
;

%inline match_cases(X):
| pe = bar_list1(separated_pair(mk_pat, LARROW, X))
    { pe }
;

mk_pat:
| p = ppat_desc { mk_ppat ($startpos, $endpos) p }
| LPAR p = mk_pat RPAR { p }
;

ppat_desc:
| PWILD
    { PWild }
| LSQUARE RSQUARE
    { PCons (mk_id "Nil" ($startpos, $endpos), []) }
| LSQUARE args = separated_nonempty_list(SEMI, mk_pat) RSQUARE
    { PCons (mk_id "Cons" ($startpos, $endpos), args) }
| p = mk_pat COLONCOLON ps = mk_pat
    { PCons (mk_id "Cons" ($startpos, $endpos), [p; ps]) }
| x = lident
    { PVar x }
| c = uident p = mk_pat %prec prec_constr_appl
    { PCons (c, [p]) }
| c = uident
  args = loption(delimited(LPAR,comma_list2(mk_pat),RPAR))
    { PCons (c, args) }
;

atom:
| a = atom_desc { mk_atom ($startpos, $endpos) a }
| a = atom_     { a }
;

atom_:
| a = atom_arg { a }
| a = atom_pattern { a }
  
atom_arg:
| LPAR a = atom RPAR { a }
| id = lident        { mk_atom ($startpos, $endpos) (AId id) }
| c = CONSTANT       { mk_atom ($startpos, $endpos) (ACst c) }
;

atom_desc:
| FUN x = lident LARROW e = expr { AFun (x, e) }
| e1 = expr o = op e2 = expr     { ABinop (e1, o, e2) }

atom_pattern:
| LSQUARE RSQUARE    { mk_nil ($startpos, $endpos) }
| a1 = atom_ COLONCOLON a2 = atom_
    { mk_cons ($startpos, $endpos) a1 a2 }
| c = uident a = atom_ %prec prec_constr_appl
    { mk_atom ($startpos, $endpos) (ACons (c, [a])) }
| c = uident 
  args = loption(delimited(LPAR,comma_list2(atom_),RPAR))
    { mk_atom ($startpos, $endpos) (ACons (c, args)) }
;

%inline op:
| ADD   { OPAdd }
| MINUS { OPMinus }
| MULT  { OPMult }
| DIV   { OPDiv }
| EQUAL { OPEq }
| LE    { OPLe }
;

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }
;

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }
;

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }
;

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }
;

bar_list1(X):
| ioption(PIPE) ; xl = separated_nonempty_list(PIPE, X) { xl }
;

comma_list2(X):
| X COMMA comma_list1(X) { $1 :: $3 }
;

comma_list1(X):
| separated_nonempty_list(COMMA, X) { $1 }
;
