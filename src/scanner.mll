(* Ocamllex scanner for Music-Mike *)

{
	open Parser
	type is_pat = NO | RHYTHM | PITCH
	let state_ref = ref NO
}


rule token pat = parse
  [' ' '\t' '\r' '\n'] { token pat lexbuf } (* Whitespace *)
| "/*"     { comment pat lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '|'      { BAR }
| "p:["    { pat:= PITCH; PLBRACKET }
| "r:["    { pat := RHYTHM; RLBRACKET }
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
| '"' { let buffer = Buffer.create 1 in STRING (stringl pat buffer lexbuf) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a' 'c'-'u' 'w'-'z'] | ['a' 'c'-'u' 'w'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'Z'] | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9''_']* as lxm { FID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment pat = parse
  "*/" { token pat lexbuf }
| _    { comment pat lexbuf }

and stringl pat buffer = parse
 | '"' { Buffer.contents buffer }
 | "\\t" { Buffer.add_char buffer '\t'; stringl pat buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl pat buffer lexbuf }
 | '\\' '"' { Buffer.add_char buffer '"'; stringl pat buffer lexbuf }
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl pat buffer lexbuf }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl pat buffer lexbuf }

and rhythmlist pat = parse
[' ' '\t' '\r' '\n'] { rhythmlist pat lexbuf } (* Whitespace *)
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']* as lxm { FLITERAL(float_of_string lxm) }
| 'q'      { FLITERAL(1.0)}
| 'w'      { FLITERAL(4.0) }
| 'h'      { FLITERAL(2.0) }
| 't'      { FLITERAL(0.33) }
| 'e'      { FLITERAL(0.5) }
| 's'      { FLITERAL(0.25) }
| ']'      { pat := NO; RBRACKET }

and pitchlist pat = parse
[' ' '\t' '\r' '\n'] { pitchlist pat lexbuf } (* Whitespace *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| '^'      { OUP }
| 'v'      { ODOWN }
| 'b'      { FLAT }
| '#'      { OCTOTHORPE }
| ']'      { pat := NO; RBRACKET }

{
	let next_token lexbuf = match !state_ref with
	    | NO -> token state_ref lexbuf
	    | RHYTHM -> rhythmlist state_ref lexbuf
	    | PITCH -> pitchlist state_ref lexbuf
}
