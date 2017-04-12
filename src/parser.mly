%{
    open Ast
    let first  (a,_,_) = a;;
    let second (_,a,_) = a;;
    let third  (_,_,a) = a;;
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET PLBRACKET RLBRACKET LTUPLE RTUPLE
%token OUP ODOWN FLAT OCTOTHORPE RHYTHMDOT DOTLBRACKET
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT FPLUS FMINUS FTIMES FDIVIDE CONCAT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF THEN ELSE WHILE INT BOOL VOID
%token TYP DEF FOR
%token <int> LITERAL
%token <float> FLITERAL
%token <float> RFLITERAL
%token <string> STRING
%token <string> ID FID
%token EOF

%right ASSIGN
%right call
%right IF
%left OR
%left AND
%left EQ NEQ
%nonassoc LT GT LEQ GEQ
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIVIDE FTIMES FDIVIDE
%left OUP ODOWN FLAT OCTOTHORPE RHYTHMDOT
%right RBRACKET
%left LBRACKET
%left CONCAT
%right NOT NEG


%start program
%type <Ast.program> program

%%

/* "A program consists of a list of declarations, aka `decls`"*/
program:
  stmts EOF             { $1 }

stmts:
                        { [], [] ,[] }
  | stmts expr          { ($2 :: first $1), second $1, third $1 }
  | stmts fdecl         { first $1, ($2 :: second $1), third $1 }
  | stmts tdecl         { first $1, second $1, ($2 :: third $1) }



/* "A function declaration `fdecl` consists of
    a Function Identifier `FID` - string w/ first letter capitalized
    a list of formals `formals_list`
    a body which consists of an `expr` expression "*/
fdecl:
   DEF FID formals_list ASSIGN expr
     { { ident = $2;
	       formals = List.rev($3);
	       body = $5 } }

tdecl:
   TYP ID ASSIGN LBRACE assign_list RBRACE
     { { typename = $2; members = $5 } }

literals:
    LITERAL          { Literal($1) }
  | FLITERAL         { FloatLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | LPAREN RPAREN    { Unit }
  | ID               { ID($1) }
  | STRING           { String($1) }

expr:
    literals  { $1 }
  | binop     { $1 }
  | unop      { $1 }
  | primaries { $1 }
  | LBRACKET expr_list RBRACKET  { List(List.rev($2)) }
  | ID DOTLBRACKET LITERAL RBRACKET  { Subset($1, $3) }
  | PLBRACKET expr_list RBRACKET { PList($2) }
  | rhythm { RList($1) }
/*| LTUPLE expr_list RTUPLE      { Tuple($2) }*/
  | expr CONCAT expr  { Concat($1, $3) }
  | IF expr THEN expr ELSE expr
      %prec IF
      { If($2, $4, $6) }
rhythm:
  | RFLITERAL {[$1]}
  | rhythm RFLITERAL {$2 :: FloatLit($1)}

binop:
  | expr PLUS    expr { Binop($1, Add,   $3) }
  | expr MINUS   expr { Binop($1, Sub,   $3) }
  | expr TIMES   expr { Binop($1, Mult,  $3) }
  | expr DIVIDE  expr { Binop($1, Div,   $3) }
  | expr FPLUS   expr { Binop($1, FAdd,  $3) }
  | expr FMINUS  expr { Binop($1, FSub,  $3) }
  | expr FTIMES  expr { Binop($1, FMult, $3) }
  | expr FDIVIDE expr { Binop($1, FDiv,  $3) }
  | expr EQ      expr { Binop($1, Equal, $3) }
  | expr NEQ     expr { Binop($1, Neq,   $3) }
  | expr LT      expr { Binop($1, Less,  $3) }
  | expr LEQ     expr { Binop($1, Leq,   $3) }
  | expr GT      expr { Binop($1, Greater, $3) }
  | expr GEQ     expr { Binop($1, Geq,   $3) }
  | expr AND     expr { Binop($1, And,   $3) }
  | expr OR      expr { Binop($1, Or,    $3) }

unop:
/*| MINUS expr %prec NEG { Preop(Neg, $2) }  */
  | NOT expr             { Preop(Not, $2) }
  | expr RHYTHMDOT       { Postop($1, Rhythmdot) }
  | expr OCTOTHORPE      { Postop($1, Sharp) }
  | expr FLAT            { Postop($1, Flat) }
  | OUP  expr            { Preop(OctaveUp, $2) }
  | ODOWN expr           { Preop(OctaveDown, $2) }

    /* stuff that should be on same level as expressions */
primaries:
    /*block { $1 }*/
    LBRACE semi_list RBRACE { Block($2) }
  | FID LPAREN actuals_list RPAREN SEMI   { Call($1, $3) }
  | assign SEMI { $1 }

assign:
  | ID ASSIGN expr          { Assign($1, $3) }

assign_list:
    assign                  { [$1] }
  | assign_list assign      { $2 :: $1 }

expr_list:
   /*nothing*/              { [] }
  | expr_list expr          { $2 :: $1 }

/*block:
    LBRACE semi_list RBRACE { Block($2) } */

semi_list:
    expr SEMI               { [$1] }
  | semi_list expr SEMI     { $2 :: $1 }

formals_list:
    ID                      { [$1] }
  | formals_list ID         { $2 :: $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list expr       { $2 :: $1 }
