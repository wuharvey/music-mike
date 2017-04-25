 
    let first  (a,_,_) = a;; 
    let second (_,a,_) = a;; 
    let third  (_,_,a) = a;; 

(* Abstract Syntax Tree and functions for printing it *)

type op = Add | FAdd | Sub | FSub | Mult | FMult | Div | FDiv | Equal | Neq | Less | Leq | Greater | Geq | And | Or 

type preop = Neg | Not | FNeg 

type postop = Rhythmdot 

type typ = Int | Bool | Void | Float | String | Pitch

type bind = typ * string

type expr =
    Literal of int
  | FloatLit of float  
  | BoolLit of bool
  | ID of string
  | String of string
  | Binop of expr * op * expr
  | Preop of preop * expr
  | Postop of expr * postop
  | Assign of string * expr
  | Call of string * expr list      
  | If of expr * expr * expr
  | Subset of string * int
  | List of expr list
  | ChordList of ((int * expr * int) list)  list (*PList --> "list of chords"*)
  | RList of expr list
  | Block of expr list
  | Concat of expr * expr
  | Noexpr
  | Unit

type func_decl = {
    ident : string;
    formals : string list;
    body : expr;
  }

type type_decl = {
    typename : string;
    members  : expr list;
  }

type program = expr list * func_decl list * type_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | FAdd -> "+."
  | Sub -> "-"
  | FSub -> "-."
  | Mult -> "*"
  | FMult -> "*."
  | Div -> "/"
  | FDiv -> "/."
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"





let string_of_preop = function 
    Neg -> "-"
  | Not -> "!"
  | FNeg -> "-"
(*  | OctaveUp -> "^"
  | OctaveDown -> "v"

let string_of_postop = function
    Sharp -> "#"
  | Flat -> "b"
  | Rhythmdot -> "o"
*)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ID(s) -> s
  | String(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Preop(o, e) -> string_of_preop o ^ string_of_expr e
(*| Postop(e, o) -> string_of_expr e ^ string_of_postop o *)
  | Assign(v, e) -> "Assign(" ^ v ^ " = " ^ (string_of_expr e) ^ ")"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Subset(s, i) -> s ^ ".[" ^ string_of_int i ^ "]"
  | List(es) -> "[ " ^ String.concat " " (List.map string_of_expr es) ^ " ]"
  | ChordList(cs) -> "broken" (*let string_of_pitch i j k = string_of_int i ^ string_of_expr j^ string_of_int k in
		let rec string_of_chord el= function 
	          [] -> ""
	        | p :: ps -> "(" ^ string_of_pitch( fst p second p third p) ^ ")" ^ "|" ^ string_of_chord ps
	      in "p:[ " ^ String.concat " " (List.map string_of_chord  cs) ^ " ]" *)
  | Block(es) -> "{ " ^ String.concat " " (List.map string_of_expr es) ^ " }"
  | Concat(e1, e2) -> string_of_expr e1 ^ "@" ^ string_of_expr e2
  | Noexpr -> ""
  | Unit -> "()"



let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | String -> "string"
  | Pitch -> "pitch"

let string_of_func_decl fdecl =  
  "def " ^ fdecl.ident ^ " " ^ String.concat " " fdecl.formals ^ "\n" ^ (string_of_expr fdecl.body) ^ "\n"

let string_of_type_decl tdecl = 
  "type " ^ tdecl.typename ^ "= {\n" ^ String.concat "\n" (List.map string_of_expr tdecl.members) ^ "}\n"


let string_of_program (exprs, functions, structs) =
  "TYPE DECLS: " ^ String.concat "" (List.map string_of_type_decl structs) ^ "\n" ^ 
  "FUN DECLS: " ^ String.concat "" (List.map string_of_func_decl functions) ^ "\n" ^
  "EXPRESSIONS: " ^ String.concat "\n" (List.map string_of_expr exprs) ^ "\n"


