(* Ocamllex scanner for Music-Mike *)

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
| "p:["    { PLBRACKET }
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
| 'o'      { RHYTHMDOT }
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
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "unit"   { UNIT }
| "true"   { TRUE }
| "false"  { FALSE }
| "fun"    { FUN }
| "set"    { SET }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLITERAL(float_of_string lxm) }
| "\""['a'-'z' 'A'-'Z']*"\""    as lxm { STRING(lxm) } 
| 'q'      { FLITERAL(1.0) }
| 'w'      { FLITERAL(4.0) }
| 'h'      { FLITERAL(2.0) }
| 't'      { FLITERAL(0.33) }
| 'e'      { FLITERAL(0.5) }
| 's'      { FLITERAL(0.25) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'d' 'f' 'g' 'i'-'n' 'p' 'r' 'u' 'v' 'x'-'z'] | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'D' 'F' 'G' 'I'-'N' 'P' 'R' 'U' 'V' 'X'-'Z'] | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9'
'_']* as lxm { FID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }