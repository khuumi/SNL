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
%nonassoc DO TO
%right ASSIGN
%left AND OR
%right NOT
%left EQ NEQ LT GT LEQ GEQ
%right OF
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


/* Matches NEWLINE* */
opt_nl:
    /* nothing */ %prec NOCOMMA { }
  | multi_nl      %prec NOCOMMA { }


/* Matches NEWLINE+ */
multi_nl:
    NEWLINE          %prec COMMA { }
  | multi_nl NEWLINE %prec COMMA { }


/* int, float, bool, string literals. */
constant:
    INT    { Int($1) }
  | FLOAT  { Float($1) }
  | TRUE   { Bool(true) }
  | FALSE  { Bool(false) }
  | STRING { String($1) }


/* Ids may be local or global. */
ids:
    ID       { Id($1, Global) }
  | LOCAL ID { Id($2, Local) }


/* exprs are the basic building blocks of programs.
 * No newlines are allowed inside.*/
expr:
    constant                   { Constant($1) }
  | LPAREN expr RPAREN         { $2 }
  | ids                        { $1 }
  | LBRACKET expr_seq RBRACKET { List($2) }
  | expr ASSIGN expr           { Assign($1, $3) }
  | math                       { $1 }
  | logic                      { $1 }
  | recipe_app                 { $1 }
  | RETURN expr                { Return($2) }
  | NEXT ID                    { Next($2) }
  | INPUT                      { Input }
  | expr OF ids                { Access($1, $3) }


/* Mathematical expressions. */
math:
    expr PLUS   expr        { Binop($1, Add, $3) }
  | expr MINUS  expr        { Binop($1, Sub, $3) }
  | expr TIMES  expr        { Binop($1, Mult, $3) }
  | expr DIVIDE expr        { Binop($1, Div, $3) }
  | MINUS expr %prec UMINUS { Unop(Negate, $2) }


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
    expr %prec NOCOMMA          { [$1] }
  | expr_seq_builder COMMA expr { $3 :: $1 }


/* Applying recipes. */
recipe_app:
    DO ID TO expr_seq_builder %prec NOCOMMA { Call($2, List.rev $4) }
  | DO ID                                   { Call($2, []) }


/* A statement is either an expression or an if-else construct. */
stmt:
    expr %prec NOELSE { Expr($1) }
  | IF expr multi_nl LPAREN block_builder RPAREN
    ELSE opt_nl LPAREN block_builder RPAREN
      { If($2, Block(List.rev $5), Block(List.rev $10)) }
  | IF expr multi_nl LPAREN block_builder RPAREN %prec NOELSE
      { If($2, Block(List.rev $5), Block([])) }


/* A block is a sequence of expr separated by newlines that appears in an
   if statement. */
block_builder:
    stmt                      { [$1] }
  | block_builder multi_nl stmt { $3 :: $1 }


stage_body:
    stmt                   { [$1] }
  | stage_body opt_nl stmt { $3 :: $1 }


stage:
    ID COLON multi_nl stage_body opt_nl DONE       { { sname = $1;
                                                       body = List.rev $4;
                                                       is_start = false } }
  | START ID COLON multi_nl stage_body opt_nl DONE { { sname = $2;
                                                       body = List.rev $5;
                                                       is_start = true } }


formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }


stage_seq:
    stage                  { [$1] }
  | stage_seq opt_nl stage { $3 :: $1 }


recipe:
    RECIPE ID COLON multi_nl
    stage_seq opt_nl DONE
      { { rname = $2;
          formals = [];
          body = List.rev $5; } }
  | RECIPE ID TO formal_list COLON multi_nl
    stage_seq opt_nl DONE
      { { rname = $2;
          formals = List.rev $4;
          body = List.rev $7; } }


program_body:
    stage opt_nl               { { recipes = [];
                                   stages = [$1]; } }
  | recipe opt_nl              { { recipes = [$1];
                                   stages = []; } }
  | stage opt_nl program_body  { { recipes = $3.recipes;
                                   stages = $1 :: $3.stages; } }
  | recipe opt_nl program_body { { recipes = $1 :: $3.recipes;
                                   stages = $3.stages; } }


program:
    /* nothing */         { { recipes = [];
                              stages = []; } }
  | program_body          { $1 }
  | multi_nl program_body { $2 }
