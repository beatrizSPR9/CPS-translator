{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string
    
  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["let", LET; "if", IF; "then", THEN; "else", ELSE;
       "in", IN; "begin", BEGIN; "end", END;
       "match", MATCH; "with", WITH; "fun", FUN;
       "true", CONSTANT (CBool true);
       "false", CONSTANT (CBool false);
       "assert", ASSERT; ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}

let letter = ['a'-'z']
let capital = ['A'-'Z']
let digit = ['0'-'9']
let lident = (letter | '_') (letter | digit | '_')*
let uident = capital (letter | capital | digit | '_')*
let integer = digit+
let space = ' ' | '\t' 

rule token = parse
  | '\n' | '\r'  { new_line lexbuf; token lexbuf }
  | space+       { token lexbuf }
  | lident as id { id_or_kwd id }
  | uident as id { UIDENT id }
  | integer as s { CONSTANT (CNum (int_of_string s)) }
  | '+'          { ADD }
  | '-'          { MINUS }
  | "//"         { DIV }
  | '='          { EQUAL }
  | '*'          { MULT }
  | "<="         { LE }
  | "->"         { LARROW }
  | '('          { LPAR }
  | ')'          { RPAR }
  | ','          { COMMA }
  | ';'          { SEMI }
  | '|'          { PIPE }
  | "::"         { COLONCOLON }
  | '['          { LSQUARE }
  | ']'          { RSQUARE }
  | "(*"         { comment lexbuf; token lexbuf }
  | eof          { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)"         { () }
  | "(*"         { comment lexbuf; comment lexbuf }
  | '\n' | 'r'   { new_line lexbuf; comment lexbuf }
  | eof          { raise (Lexing_error ("unfinished comment")) }
  | _            { comment lexbuf }
