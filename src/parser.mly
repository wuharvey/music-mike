%{
    open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET PLBRACKET RLBRACKET LTUPLE RTUPLE 
%token OUP ODOWN FLAT OCTOTHORPE RHYTHMDOT DOTLBRACKET BAR

%token PLUS MINUS TIMES DIVIDE ASSIGN NOT FPLUS FMINUS FTIMES FDIVIDE CONCAT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF THEN ELSE WHILE INT BOOL VOID
%token TYP DEF FOR
%token <int> LITERAL
%token <float> FLITERAL
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

/* "A program consists of a list of statements, aka `stmts`"*/

program:        

  stmts EOF             { $1 }



/* "stmts is a tuple with the first field being a list of expressions (expr),
the second field being a list of function declarations (fdecl), and the 
third field being a list of type declaratiosn (tdecl)" */

stmts:
                        { [] }
  | stmts expr  SEMI    { $2 :: $1 }

                            
/* "A function declaration `fdecl` consists of 
    a keyword 'Def'

    a Function Identifier `FID` - string w/ first letter capitalized
    a list of formals `formals_list`
    a body which consists of an `expr` expression "*/

fdecl: 
    DEF FID formals_list ASSIGN expr { Fun($2, $3, $5) }  
    
/* "expressions always return a value and consists of:
	literals-basic types
	binop-binary operator
	unop-unary operators
	primaries-miscelaneous pool (list type, assignment, etc. "*/

expr:
    literals  { $1 }
  | binop     { $1 }
  | unop      { $1 }
  | primaries { $1 }
  | fdecl     { $1 }
  | LPAREN expr RPAREN { $2 }
  
literals:
    LITERAL          { Literal($1) }
  | FLITERAL         { FloatLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | LPAREN RPAREN    { Unit }
  | ID               { ID($1) }
  | STRING           { String($1) }



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



primaries:
	/* "Block of expressions" */

    LBRACE semi_list RBRACE { Block(List.rev($2)) }
	/* "Calling a function "*/
  | FID LPAREN actuals_list RPAREN   { Call(ID($1), $3) }
	/* "Assigning a value to an variable"*/       
  | assign          { $1 }
	/* "list of expressions of same type (enforced in semant.ml)" */
  | LBRACKET expr_list RBRACKET  { List(List.rev($2)) }
	/* "list of chords" */
  | PLBRACKET pxpr_list RBRACKET { ChordList(List.rev($2)) }  
       /*"list of rhythms"*/
  | RLBRACKET expr_list RBRACKET { RList(List.rev($2)) }
       /* "tuple of expressions with different types (enforced in semant.ml)" */
/*  | LTUPLE expr_list RTUPLE      { Tuple($2) }*/
	/* "concatanating 2 lists (enforced in semant.ml)" */
  | expr CONCAT expr  { Concat($1, $3) }
	/* "If, then else "*/
  | IF expr THEN expr ELSE expr  
      %prec IF
      { If($2, $4, $6) }
	/* "getting an element from a list/tuple/pitchlist" */
  | ID DOTLBRACKET expr RBRACKET  { Subset(ID($1), $3) }
 


        /* "Assigning a value to an variable"*/
assign:
    ID ASSIGN expr          { Assign($1, $3) }

/*" List of assingments (a=b) used in type declaration " */  

assign_list: 

    assign                  { [$1] }
  | assign_list assign      { $2 :: $1 }


/* "List of whitespace separated expressions used in
	-Lists
	-Tuples" */	
expr_list:
   /*nothing*/              { [] }
  | expr_list expr          { $2 :: $1 }



/* "List of semicolon separated expressions used in block" */

semi_list: 
    expr SEMI               { [$1] }  
  | semi_list expr SEMI     { $2 :: $1 } 



/* "List of formal arguments used in function declaration" */   

formals_list:
    /*nothing*/                      { [] }
  | formals_list ID         { $2 :: $1 }


/* "List of actual arguments used in function calls" */

actuals_list:
    /*nothing*/                    { [] }
  | actuals_list expr       { $2 :: $1 }


/* "List of whitespace separated chords(simultaneous pitches) 
used in Plist (pitch list) "*/

pxpr_list:
    chord                   { [Chord($1)] }
  | pxpr_list chord         { Chord($2) :: $1 }


/* "List of simulateous pitches" */

chord:
    pitch                   { [$1] }         
  | chord BAR pitch         { $3 :: $1 }
/*p:[3|5|6  3  ^^3#|9bb]*/


/* "Tuple consisting of 3 fields: 
	prefield-a list of ints representing '^' and 'v' as  '1' and '-1'
  	an int representing scale degree inputed by user
  	postfield-a list of ints representing '#' and 'b' as '1' and '-1' "*/

pitch:
    prefield expr postfield { Pitch($1, $2, $3) }


/*"a list of ints representing '^' and 'v' as  '1' and '-1' respectively" */

prefield:
/* nothing */               { 0 }  
  | prefield OUP            { $1+1 }
  | prefield ODOWN          { $1-1 }


/* "a list of ints representing '#' and 'b' as '1' and '-1' "*/

postfield:
/*nothing*/                 { 0 }
  | postfield OCTOTHORPE    { $1+1 }
  | postfield FLAT          { $1-1 }
 
