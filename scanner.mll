(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '[|'     { LINDEX }
| '|]'     { RINDEX }
| "p:["    { PLBRACKET }
| "r:["    { RLBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| "+."     { FPLUS }
| "-."     { FMINUS }
| "*."     { FTIMES }
| "/."     { FDIVIDE }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { OUP }
| 'v'      { ODOWN }
| 'b'      { FLAT }
| '#'      { OCTOTHORPE }
| '@'      { CONCAT }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "<<"     { LTUPLE }
| ">>"     { RTUPLE }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "fun"    { FUN }
| ['0'-'9']+'.'['0'-'9']+ as lxm { FLITERAL(float_of_string lxm) }
| 'q'      { FLITERAL(1.0) }
| 'w'      { FLITERAL(4.0) }
| 'h'      { FLITERAL(2.0) }
| 't'      { FLITERAL(0.33) }
| 'e'      { FLITERAL(0.5) }
| 's'      { FLITERAL(0.25) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
