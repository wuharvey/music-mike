open Ast

module StringMap = Map.Make(String)
type environment = typ StringMap.t
(* TODO: Right now everything is a global *)

let letter = ref (Char.code 'a')

let new_type () =
  let c1 = !letter in
  incr letter; T(Char.escaped (Char.chr c1));
   
let keywords = ["if"; "then"; "else"; "true"; "false"; "def"];; 

let rec annotate_expr exp env = 
    match exp with
  | Literal(n)  -> ALiteral(n, TInt)
  | FloatLit(n) -> AFloatLit(n, TFloat)
  | BoolLit(n)  -> ABoolLit(n, TBool)
  | ID(n)       -> if StringMap.mem n env 
                   then AID(n, StringMap.find n env)
                   else raise Not_found
  | Binop(e1, op, e2) -> 
    let ae1 = annotate_expr e1 env 
    and ae2 = annotate_expr e2 env
    and ntyp = new_type () in
    ABinop(ae1, op, ae2, ntyp)
  | Preop(preop, e) -> 
    let ae = annotate_expr e env
    and ntyp = new_type () in
    APreop(preop, ae, ntyp)
  | Postop(e, postop) ->
    let ae = annotate_expr e env
    and ntyp = new_type () in 
    APostop(ae, postop, ntyp)
  | Assign(name, e) ->
    if StringMap.mem name env
    then raise (Failure "Reassignment")
    else let ntyp = new_type () in 
      StringMap.add name ntyp env in
    let ae = annotate_expr e env in 
    AAssign(name, e, ntyp)
  | Call(name, e_list) ->
    let anno one_e = annotate_expr one_e env in
    let ae_list = List.map anno e_list in
    ACall(name, e_list, new_type ())
  | Fun(name, formals, e) ->
    (* TODO: Check for keywords being passed as args. *)
    let ae = annotate_expr e env and
    let t = StringMap.find name env in 
    AFun(name, formals, 
  | _ -> AUnit

let rec type_of ae = 
    match ae with
  | ALiteral(_,t)   -> t
  | AFloatLit(_,t)  -> t
  | ABoolLit(_,t)   -> t
  | AID(_,t)        -> t
  | ABinop(_,_,_,t) -> t
  | APreop(_,_,t)   -> t
  | APostop(_,_,t)  -> t
  | AAssign(_,_,t)  -> t
  | ACall(_,_,t)    -> t
  | AFun(_,_,t)     -> t
  | _               -> Unit 

let rec collect_expr ae = 
    match ae with
  | ALiteral(_)     -> []
  | ABoolLit(_)     -> []
  | AVal(_)         -> []
  | ABinop(ae1, op, ae2, t) ->
     let t1 = type_of ae1
     and t2 = type_of ae2 in
     let con = match op with 
       | Add | Sub | Mult | Div -> [(t1, Int); (t2, Int); (t, Int)]
       | FAdd | FSub | FMult | FDiv ->
         [(t1, Float); (t2, Float); (t, Float)]
       | Equal | Greater | Less | Geq | Leq ->
         [(t1, t2); (t, Bool)]
       | And | Or -> [(t1, Bool); (t2, Bool); (t, Bool)] 
     in 
     (collect_expr e1) @ (collect_expr e2) @ con 
  | AFun(name, 
  | ACall(name, arg, t) -> match type_of name 
  |  





let rec substitute u x t = 
    match t with 
    | TUnit | TInt | TBool | TFloat | TPitch -> t
    | TType(c) -> if c = x then u else t
    | TFun(t1, t2) -> TFun(List.map (substitute u x) t1, substitute u x t2)
    | TList(t) -> TList(substitute u x t)
;;

let apply subs t = 
    List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

let rec unify constraints = 
    match 
