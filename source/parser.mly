%{ open Ast %}

%token COMMENT COLON LPAREN RPAREN LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE AND OR
%token START END NEXT DO TO OF LOCAL
%token <int> INT
%token <float> FLOAT
%token <string> ID STRING
%token NEWLINE EOF

%nonassoc COMMA
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOTO
%right DO TO
%right ASSIGN
%left AND OR
%right NOT
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start stmt
%type <Ast.stmt> stmt

%%

constant:
    INT    { Int($1) }
  | FLOAT  { Float($1) }
  | STRING { String($1) }

sequence:
    /* nothing */    %prec COMMA { [] }
  | sequence_builder %prec COMMA { List.rev $1 }

sequence_builder:
    expr                        %prec COMMA { [$1] }
  | sequence_builder COMMA expr %prec COMMA { $3 :: $1 }

expr:
    constant { Constant($1) }
  | LPAREN expr RPAREN { $2 }
  | ID { Id($1) }
  | LBRACKET sequence RBRACKET { List($2) }
  | ID ASSIGN expr { Assign($1, $3) }
  | math { $1 }
  | logic { $1 }
  | DO ID TO sequence { Call($2, $4) }
  | DO ID %prec NOTO { Call($2, []) }

math:
    expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | MINUS expr %prec UMINUS  { Unop(Negate, $2) }

logic:
    expr EQ  expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT  expr { Binop($1, Lt, $3) }
  | expr LEQ expr { Binop($1, Leq, $3) }
  | expr GT  expr { Binop($1, Gt, $3) }
  | expr GEQ expr { Binop($1, Geq, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR  expr { Binop($1, Or, $3) }
  | NOT expr      { Unop(Not, $2) }

stmt:
    expr NEWLINE { Expr($1) }
