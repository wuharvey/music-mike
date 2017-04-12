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
| "r:"    { listl lexbuf }
| ".["	   { DOTLBRACKET }
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
(*| "return" { RETURN }*)
| "int"    { INT }
| "bool"   { BOOL }
(*| "float"  { FLOAT }*)
(*| "unit"   { UNIT }*)
| "true"   { TRUE }
| "false"  { FALSE }
| "def"    { DEF }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLITERAL(float_of_string lxm) }
| '"' { let buffer = Buffer.create 1 in STRING (stringl buffer lexbuf) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'p'] | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'Z'] | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9''_']* as lxm { FID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and stringl buffer = parse
 | '"' { Buffer.contents buffer }
 | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
 | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }

and listl = parse
| '['      { RSTART }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { RFLITERAL(float_of_string lxm) }
| 'q'      { RFLITERAL }
| 'w'      { RFLITERAL }
| 'h'      { RFLITERAL }
| 't'      { RFLITERAL }
| 'e'      { RFLITERAL }
| 's'      { RFLITERAL }
| ']'      { let _ = RBRACKET in token lexbuf }
