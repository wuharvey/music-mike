/* Ocamlyacc parser for Music-Mike*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET PLBRACKET RLBRACKET LTUPLE RTUPLE 
%token OUP ODOWN FLAT OCTOTHORPE
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT FPLUS FMINUS FTIMES FDIVIDE CONCAT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF THEN ELSE FOR WHILE INT BOOL VOID FUN 
%token <int> LITERAL
%token <float> FLITERAL
%token <string> ID FID
%token EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIVIDE FTIMES FDIVIDE
%left OUP ODOWN FLAT OCTOTHORPE
%left CONCAT
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

fdecl: /* formals are not optional */
   FID formal_list ASSIGN expr_list
     { { ident = $1;
	 formals = $2;
	 body = List.rev $4 } }
	/* Syntax question: why are there two braces? */

formal_list:
    ID                   { $1 }
  | formal_list ID { ($2) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr FPLUS   expr { Binop($1, FAdd,   $3) }
  | expr FMINUS  expr { Binop($1, FSub,   $3) }
  | expr FTIMES  expr { Binop($1, FMult,  $3) }
  | expr FDIVIDE expr { Binop($1, FDiv,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | FMINUS expr %prec NEG { Unop(FNeg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
