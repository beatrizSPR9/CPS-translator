open Cps
open Ast
open Format
open Lexing

let mk_id x = {
  id_name = x;
  id_loc = Lexing.dummy_pos, Lexing.dummy_pos
}

let dummy = Lexing.dummy_pos, Lexing.dummy_pos

let mk_atom atom_loc atom_desc = { atom_loc; atom_desc }

let var_id id = mk_atom dummy (AId (mk_id id))

let file = Sys.argv.(1)

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    (* parsing program *)
    let p = Parser.program Lexer.token lb in
    close_in c;
    (* converting into CPS mode *)
    let cps_p = Convert.program p (var_id "k") in
    eprintf "%a@." Print.pp_program cps_p
  with
  | Lexer.Lexing_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s@." s;
      exit 1
  | Parser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error@.";
      exit 1
