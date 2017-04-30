(* Abstract Syntax Tree and functions for printing it *)

type op = Add | FAdd | Sub | FSub | Mult | FMult | Div | FDiv | Equal | Neq | Less | Leq | Greater | Geq | And | Or 

type preop = Neg | Not | FNeg | OctaveUp | OctaveDown

type postop = Sharp | Flat | Rhythmdot 

type typ = 
    TUnit
  | TInt 
  | TBool
  | TFloat
  | TString
  | TPitch
  | TType of string
  | TList of typ
  | TFun of typ list * typ 

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
  | Call of expr * expr list      
  | If of expr * expr * expr
  | Subset of string * int
  | List of expr list
  | PList of expr list
  | RList of expr list
  | Block of expr list
  | Concat of expr * expr
  | Noexpr
  | Fun of string * string list * expr  
  | Unit

type aexpr = 
    ALiteral of int * typ
  | AFloatLit of float * typ 
  | ABoolLit of bool * typ
  | AID of string * typ
  | AString of string * typ
  | ABinop of aexpr * op * aexpr * typ
  | APreop of preop * aexpr * typ
  | APostop of aexpr * postop * typ
  | AAssign of string * aexpr * typ
  | ACall of aexpr * aexpr list * typ     
  | AIf of aexpr * aexpr * aexpr * typ
  | ASubset of string * int * typ
  | AList of aexpr list * typ
  | APList of aexpr list * typ
  | ARList of aexpr list * typ
  | ABlock of aexpr list * typ
  | AConcat of aexpr * aexpr * typ
  | ANoexpr
  | AFun of string * string list * aexpr * typ (* might be some issue with formals as string list *)
  | AUnit of typ 
  
type program = expr list 

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
  | OctaveUp -> "^"
  | OctaveDown -> "v"

let string_of_postop = function
    Sharp -> "#"
  | Flat -> "b"
  | Rhythmdot -> "o"

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
  | Postop(e, o) -> string_of_expr e ^ string_of_postop o
  | Assign(v, e) -> "Assign(" ^ v ^ " = " ^ (string_of_expr e) ^ ")"
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Subset(s, i) -> s ^ ".[" ^ string_of_int i ^ "]"
  | List(es) -> "[ " ^ String.concat " " (List.map string_of_expr es) ^ " ]"
  | PList(es) -> "p:[ " ^ String.concat " " (List.map string_of_expr es) ^ " ]"
  | Block(es) -> "{ " ^ String.concat " " (List.map string_of_expr es) ^ " }"
  | Concat(e1, e2) -> string_of_expr e1 ^ "@" ^ string_of_expr e2
  | Noexpr -> ""
  | Unit -> "()"
  | _ -> "string_of_expr not implemented for your expression yet."


let string_of_typ = function
    TInt -> "int"
  | TBool -> "bool"
  | TFloat -> "float"
  | TString -> "string"
  | TPitch -> "pitch"
  | TUnit -> "unit"
  | _ -> "String of type not implemented for this type yet"

(*
let string_of_func_decl fdecl =  
  "def " ^ fdecl.ident ^ " " ^ String.concat " " fdecl.formals ^ "\n" ^ (string_of_expr fdecl.body) ^ "\n"
*)

let string_of_program (exprs) =
  "EXPRESSIONS: " ^ String.concat "\n" (List.map string_of_expr exprs) ^ "\n"


