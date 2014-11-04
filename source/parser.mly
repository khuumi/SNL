%{ open Ast %}

%token COMMENT COLON LPAREN RPAREN LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE AND OR NOT TRUE FALSE
%token RECIPE DONE START NEXT RETURN DO TO OF LOCAL INPUT
%token <int> INT
%token <float> FLOAT
%token <string> ID STRING
%token NEWLINE EOF

%nonassoc NOCOMMA
%nonassoc COMMA
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc RETURN
%nonassoc NOTO
%nonassoc DO TO
%right ASSIGN
%left AND OR
%right NOT
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%nonassoc LPAREN RPAREN

%start expr
%type <Ast.expr> expr

%start stmt
%type <Ast.stmt> stmt

%start program
%type <Ast.program> program

%%

/* An optional newline; matches the regex '\n*' since NEWLINE is '\n+'. */
opt_nl:
    /* nothing */ { }
  | NEWLINE       { }


/* int, float, bool, string literals. */
constant:
    INT    { Int($1) }
  | FLOAT  { Float($1) }
  | TRUE   { Bool(true) }
  | FALSE  { Bool(false) }
  | STRING { String($1) }


/* exprs are the basic building blocks of programs.
 * No newlines are allowed inside.*/
expr:
    constant                   { Constant($1) }
  | LPAREN expr RPAREN         { $2 }
  | ID                         { Id($1, Global) }
  | LOCAL ID                   { Id($2, Local) }
  | LBRACKET expr_seq RBRACKET { List($2) }
  | ID ASSIGN expr             { Assign($1, $3) }
  | math                       { $1 }
  | logic                      { $1 }
  | recipe_app                 { $1 }
  | RETURN expr                { Return($2) }
  | NEXT ID                    { Next($2) }
  | INPUT                      { Input }


/* Mathematical expressions. */
math:
    expr PLUS   expr         { Binop($1, Add, $3) }
  | expr MINUS  expr         { Binop($1, Sub, $3) }
  | expr TIMES  expr         { Binop($1, Mult, $3) }
  | expr DIVIDE expr         { Binop($1, Div, $3) }
  | MINUS expr %prec UMINUS  { Unop(Negate, $2) }


/* Boolean expressions. */
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


/* A sequence is a comma-separated succession of exprs. It can be used inside
 * brackets to define a list or when defining or applying recipes. */
expr_seq:
    /* nothing */    %prec NOCOMMA { [] }
  | expr_seq_builder %prec NOCOMMA { List.rev $1 }


expr_seq_builder:
    expr %prec COMMA                        { [$1] }
  | expr_seq_builder COMMA expr %prec COMMA { $3 :: $1 }


/* Applying recipes. */
recipe_app:
    DO ID TO expr_seq { Call($2, $4) }
  | DO ID %prec NOTO  { Call($2, []) }


/* A statement is either an expression or an if-else construct. */
stmt:
    expr NEWLINE { Expr($1) }
  | block NEWLINE { Block(List.rev $1) }
  | IF expr NEWLINE stmt ELSE opt_nl stmt { If($2, $4, $7) }
  | IF expr NEWLINE stmt %prec NOELSE { If($2, $4, Block([])) }


/* A block is simply a sequence of expr separated by newlines and surrounded
 * by parentheses. */
block:
    LPAREN block_builder RPAREN %prec COMMA { $2 }


block_builder:
    expr %prec COMMA                       { [$1] }
  | block_builder NEWLINE expr %prec COMMA { $3 :: $1 }


stage_body:
    stmt            { [$1] }
  | stage_body stmt { $2 :: $1 }


stage:
    ID COLON NEWLINE stage_body DONE       { { sname = $1;
                                               locals = [];
                                               body = List.rev $4;
                                               is_start = false } }
  | START ID COLON NEWLINE stage_body DONE { { sname = $2;
                                               locals = [];
                                               body = List.rev $5;
                                               is_start = true } }


formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }


stage_seq:
    stage           { [$1] }
  | stage_seq stage { $2 :: $1 }


recipe:
    RECIPE ID COLON stage_seq DONE                { { rname = $2;
                                                      formals = [];
                                                      body = List.rev $4;
                                                      globals = [] } }
  | RECIPE ID TO formal_list COLON stage_seq DONE { { rname = $2;
                                                      formals = List.rev $4;
                                                      body = List.rev $6;
                                                      globals = [] } }


program:
    /* nothing */  { { recipes = [];
                       stages = [] } }
  | program stage  { { recipes = $1.recipes;
                       stages = $2 :: $1.stages } }
  | program recipe { { recipes = $2 :: $1.recipes;
                       stages = $1.stages } }
