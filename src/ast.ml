(* Abstract Syntax Tree and functions for printing it *)

type op = Add | FAdd | Sub | FSub | Mult | FMult | Div | FDiv | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type preop = Neg | Not | FNeg 

type postop = Rhythmdot 

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
  | ChordList of ((int * expr * int) list)  list (*PList --> "list of chords"*)
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

type inferred_program = aexpr list

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
(*| OctaveUp -> "^"
  | OctaveDown -> "v" *)

let string_of_postop = function
(*    Sharp -> "#"
  | Flat -> "b" *)
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
(*| Postop(e, o) -> string_of_expr e ^ string_of_postop o *)
  | Assign(v, e) -> "Assign(" ^ v ^ " = " ^ (string_of_expr e) ^ ")"
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Subset(s, i) -> s ^ ".[" ^ string_of_int i ^ "]"
  | List(es) -> "[ " ^ String.concat " " (List.map string_of_expr es) ^ " ]"

  | ChordList(cs) -> "broken" (*let string_of_pitch i j k = string_of_int i ^ string_of_expr j^ string_of_int k in
		let rec string_of_chord el= function 
	          [] -> ""
	        | p :: ps -> "(" ^ string_of_pitch( fst p second p third p) ^ ")" ^ "|" ^ string_of_chord ps
	      in "p:[ " ^ String.concat " " (List.map string_of_chord  cs) ^ " ]" *)

  | RList(es) -> "r:[ " ^ String.concat " " (List.map string_of_expr es) ^ " ]"
  | Block(es) -> "{ " ^ String.concat " " (List.map string_of_expr es) ^ " }"
  | Concat(e1, e2) -> string_of_expr e1 ^ "@" ^ string_of_expr e2
  | Noexpr -> ""
  | Unit -> "()"
  | _ -> "string_of_expr not implemented for your expression yet."

let rec string_of_typ = function
    TInt -> " [int]"
  | TBool -> " [bool]"
  | TFloat -> " [float]"
  | TString -> " [string]"
  | TPitch -> " [pitch]"
  | TUnit -> " [unit]"
  | TType(s) -> " [" ^ s ^ "]"
  | TFun(t1, t2) -> String.concat " " (List.map string_of_typ t1) ^ " ->" ^ string_of_typ t2  
  | TList(s) -> string_of_typ s ^ "list"

let rec string_of_aexpr = function
    ALiteral(l,t) -> string_of_int l ^ string_of_typ t
  | AFloatLit(f,t) -> string_of_float f ^ string_of_typ t
  | ABoolLit(true, t) -> "true" ^ string_of_typ t
  | ABoolLit(false, t) -> "false" ^ string_of_typ t
  | AID(s, t) -> s ^ string_of_typ t
  | AString(s, t) -> s ^ string_of_typ t
  | ABinop(e1, o, e2, t) ->
      string_of_aexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_aexpr e2 ^ string_of_typ t
  | APreop(o, e, t) -> string_of_preop o ^ string_of_aexpr e ^ string_of_typ t
  | APostop(e, o, t) -> string_of_aexpr e ^ string_of_postop o ^ string_of_typ t
  | AAssign(v, e, t) -> "Assign(" ^ v ^ " = " ^ (string_of_aexpr e) ^ ")" ^ string_of_typ t
  | AFun(id, f, e, t) -> "Function" ^ string_of_typ t
  | ACall(f, el, t) ->
      string_of_aexpr f ^ "(" ^ String.concat ", " (List.map string_of_aexpr el) ^ ")" ^ string_of_typ t
  | AIf(e1, e2, e3, t) -> "if " ^ string_of_aexpr e1 ^ " then " ^ string_of_aexpr
  e2 ^ " else " ^ string_of_aexpr e3 ^ string_of_typ t
  | ASubset(s, i, t) -> s ^ ".[" ^ string_of_int i ^ "]" ^ string_of_typ t
  | AList(es, t) -> "[ " ^ String.concat " " (List.map string_of_aexpr es) ^ " ]" ^ string_of_typ t
  | APList(es, t) -> "p:[ " ^ String.concat " " (List.map string_of_aexpr es) ^ " ]" ^ string_of_typ t
  | ABlock(es, t) -> "{ " ^ String.concat " " (List.map string_of_aexpr es) ^ " }" ^ string_of_typ t
  | AConcat(e1, e2, t) -> string_of_aexpr e1 ^ "@" ^ string_of_aexpr e2 ^ string_of_typ t
  | ANoexpr -> ""
  | AUnit(t) -> "()" ^ string_of_typ t
  | _ -> "[string_of_aexpr not implemented.]"

let string_of_program (exprs) =
  "EXPRESSIONS: " ^ String.concat "\n" (List.map string_of_expr exprs) ^ "\n"

let string_of_inferred (aexprs) = 
  "INFERRED EXPRS: " ^ String.concat "\n" (List.map string_of_aexpr aexprs) ^
  "\n"

