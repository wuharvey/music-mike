/* Ocamlyacc parser for Music-Mike*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET PLBRACKET RLBRACKET LTUPLE RTUPLE 
%token OUP ODOWN FLAT OCTOTHORPE RHYTHMDOT
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT FPLUS FMINUS FTIMES FDIVIDE CONCAT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF THEN ELSE FOR WHILE INT BOOL VOID FUN 
%token FOR TYP SET
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


/* "A program consists of a list of declarations, aka `decls`"*/
program:        
  decls EOF { List.rev $1 }

/* "decls consists of a `section` or a 
  `section` followed by a semicolon followed by more decls" */
decls:                       /* semicolon delimited list of sections */
    /* nothing */ { [] }
 | section        { $1 }
 | section SEMI decls  { $3 :: $1 }

/* "A section consists of either an Expression `expr`
    Function Declaration `fdecl` or 
    Type Declaration `typedecl`" */
section:							   /* expression, type declaration, or function declaration */
    expr   { $1 }
  | fdecl  { $1 }
  | typedecl { $1 }


/* "A function declaration `fdecl` consists of 
    a Function Identifier `FID` - string w/ first letter capitalized
    a list of formals `formals_list` 
    a body which consists of an `expr` expression "*/
fdecl: /* formals are not optional */
   SET FID formals_list ASSIGN expr  /* expr can go to expr_list */
     { { ident = $2;
	       formals = $3;
	       body = $5 } }
	/* Syntax question: why are there two braces? */

/* "A `expr_opt` consists of either 0 or 1 expressions  "*/
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }


expr:
    LITERAL          { Literal($1) }
  | FLITERAL         { FLiteral($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }

        /* braced stuff*/  
  | LBRACKET whitesp_list RBRACKET { $2 }
  | PBRACKET whitesp_list RBRACKET { $2 }
  | LTUPLE whitesp_list RTUPLE     { $2 }
  | LPAREN expr RPAREN { $2 }                        /* explicitly make parenthesis enclosed stuff higher than +, -, etc. */
  | expr LBRACK LITERAL RBRACK { Sub($1, $3) }		   /* subsetting  e.g. list[4], MAY NEED TO MESS WITH PRECEDENCE  */
  | LBRACKET whitesp_list RBRACKET CONCAT LBRACKET whitesp_list RBRACKET {  Concat($2, $6) }
  | PBRACKET whitesp_list RBRACKET CONCAT LBRACKET whitesp_list RBRACKET {  Concat($2, $6) }

        /* binary operations */
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

        /* general unary operators */
  | MINUS expr %prec NEG { Preop(Neg, $2) }
  | FMINUS expr %prec NEG { Preop(FNeg, $2) }
  | NOT expr         { Preop(Not, $2) }

        /* music operators */
  | expr RHYTHMDOT   { Postop($1, Rhythmdot)}
  | expr OCTOTHORPE  { Postop($1, Hashtag) }
  | expr FLAT        { Postop($1, Flat) }
  | OUP  expr        { Preop (OctaveUp, $2) }
  | ODOWN expr       { Preop (OctaveDown, $2) }

        /* miscelaneous */  
  | ID ASSIGN expr   { Assign($1, $3) }
  | FID actuals_opt { Call($1, $2) }                       /* replaced from  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } */
  | LBRACE expr_list RBRACE  { List.rev $2}                /* replaced from LBRACE expr RBRACE */
  | IF expr THEN expr ELSE expr { If($2, $4, $6) }
  | ID DOT ID   { Get($1, $3) }							  /* getting thing within user-defined type */
  | expr CONCAT expr  { Concat($1, $3) }
 
/* block of expressions */
   

/* SUGGESTED REPLACEMENT FOR whitesp_list:
    expr { $1 }
  | expr COMMA expr { $3 || $1 }
 */


expr_list: 
    expr    { $1 }
  | expr_list SEMI expr {$3 :: $1} 
   
formals_list:
  | expr                    { [$1] }
  | formals_list expr       { $3 :: $1 }   /* deleted comma delimiter */

typedecl: 
   TYP ID LBRACE expr_list RBRACE { Typedef($2, List.rev $4) }
