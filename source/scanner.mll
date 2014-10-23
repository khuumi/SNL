{ open Parser }

rule tokenize = parse
  (* Whitespace we ignore. *)
  | [' ' '\t' '\r'] { tokenize lexbuf }

  (* Comments. *)
  | "#"      { comment lexbuf }

  (* Binary operators: math, comparison, and logic. *)
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { TIMES }
  | "/"      { DIVIDE }
  | "="      { EQ }
  | "!="     { NEQ }
  | "<"      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "and"    { AND }
  | "or"     { OR }

  (* Control flow. *)
  | "if"     { IF }
  | "else"   { ELSE }

  (* Function calls. *)
  | "do"     { DO }
  | "to"     { TO }

  (* Used for grouping things and creating lists. *)
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "["      { LBRACKET }
  | "]"      { RBRACKET }
  | ","      { COMMA }

  (* Stage-related terms. *)
  | ":"      { COLON }
  | "start"  { START }
  | "end"    { END }
  | "next"   { NEXT }

  (* Other operators used with variables. *)
  | "is"     { ASSIGN }
  | "of"     { OF }
  | "local"  { LOCAL }

  (* Literals: int, float, and string. *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | '"'      { read_string (Buffer.create 17) lexbuf }

  (* Special characters we use to mark end of program/statement. *)
  | eof { EOF }
  | "\n" { NEWLINE }

  (* Anything else is an illegal character. *)
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(* Comments do not nest and are only single-line. *)
and comment = parse
  | "\n" { tokenize lexbuf }
  | _    { comment lexbuf }

(* Read in string literals. The code is from
https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
and read_string buf = parse
  | '"'       { STRING(Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      }
  | _ { raise (Failure("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure("String is not terminated")) }
