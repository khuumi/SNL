%{ open Ast %}

%token COMMENT COLON LPAREN RPAREN LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE AND OR
%token START END NEXT DO TO OF LOCAL
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token NEWLINE EOF

%nonassoc NOELSE
%nonassoc ELSE
%right DO TO
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program stage_decl { fst $1, ($2 :: snd $1) }

stage_decl:
   ID COLON stmt
     { { stage_name = $1;
	 body = $3 } }

stmt:
    expr NEWLINE { Expr($1) }
  | IF expr stmt %prec NOELSE { If($2, $3, Block([])) }
  | IF expr stmt ELSE stmt { If($2, $3, $5) }

expr:
    INT              { Int($1) }
  | FLOAT            { Float($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Lt,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Gt,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | DO ID TO LBRACKET list_contents RBRACKET { Call($2, $5) }
  | LBRACKET list_contents RBRACKET { $2 }

list_contents:
    /* nothing */ { [] }
  | list_builder  { List.rev $1 }

list_builder:
    expr               { [$1] }
  | list_builder COMMA expr { $3 :: $1 }
