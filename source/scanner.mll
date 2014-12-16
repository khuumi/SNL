{ open Parser }

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\r']
let comment = "#" [^ '\n']* "\n"
let ws_strip = (whitespace|comment|'\n')*


rule tokenize = parse
  (* Whitespace we split on. *)
    whitespace  { tokenize lexbuf }

  (* Comments. *)
  | comment  { tokenize lexbuf }

  (* Binary operators: math, comparison, and logic. *)
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { TIMES }
  | "/"    { DIVIDE }
  | "="    { EQ }
  | "!="   { NEQ }
  | "<"    { LT }
  | "<="   { LEQ }
  | ">"    { GT }
  | ">="   { GEQ }
  | "and"  { AND }
  | "or"   { OR }
  | "not"  { NOT }

  (* Control flow. *)
  | "if"             { IF }
  | ws_strip "else"  { ELSE }

  (* Function calls. *)
  | "do"  { DO }
  | "to"  { TO }

  (* Used for grouping things and creating lists. *)
  | "(" ws_strip  { LPAREN }
  | ws_strip ")"  { RPAREN }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | ","           { COMMA }

  (* Recipe- and stage-related terms. *)
  | ":"       { COLON }
  | "recipe"  { RECIPE }
  | "done"    { DONE }
  | "start"   { START }
  | "next"    { NEXT }
  | "return"  { RETURN }

  (* Other operators used with variables. *)
  | "is"     { ASSIGN }
  | "of"     { OF }
  | "local"  { LOCAL }

  (* I/O *)
  | "input"  { INPUT }

  (* Identifiers and literals (int, float, bool, string). *)
  | "true"   { TRUE }
  | "false"  { FALSE }
  | digit+ as lxm { INT(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | (digit+'.'digit*)|(digit*'.'digit+) as lxm { FLOAT(float_of_string lxm) }
  | '"'      { read_string (Buffer.create 17) lexbuf }

  (* Special characters we use to mark end of programs/statements. *)
  | eof             { EOF }
  | '\n'+ ws_strip  { NEWLINE }  (* Empty lines are collapsed. *)

  (* Anything else is an illegal character. *)
  | _ as char  { raise (Failure("illegal character " ^ Char.escaped char)) }


(* Read in string literals. The code is from
https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
and read_string buf = parse
    '"'            { STRING(Buffer.contents buf) }
  | '\\' '/'       { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'      { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'       { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'       { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'       { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'       { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'       { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'       { Buffer.add_char buf '\\';
                     Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                     read_string buf lexbuf }
  | _              { raise (Failure("Illegal string character: " ^
                                      Lexing.lexeme lexbuf)) }
  | eof            { raise (Failure("String is not terminated")) }
