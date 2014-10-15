{ open Parser }

rule token = parse
  [' ' '\t' '\r'] { token lexbuf }
| "#"      { comment lexbuf }
| ':'      { COLON }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "="      { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "is"     { ASSIGN }
| "if"     { IF }
| "else"   { ELSE }
| "start"  { START }
| "end"    { END }
| "next"   { NEXT }
| "do"     { DO }
| "to"     { TO }
| "of"     { OF }
| "local"  { LOCAL }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| '\n' { NEWLINE }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
