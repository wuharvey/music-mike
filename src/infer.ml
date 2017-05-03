open Ast

module StringMap = Map.Make(String)
type environment = typ StringMap.t
type substitutions = (typ * typ) list 
(* TODO: Right now everything is a global *)

let letter = ref (Char.code 'a');;

let new_type () = let c1 = !letter in
  incr letter; TType(Char.escaped (Char.chr c1))
;;
   
let keywords = ["if"; "then"; "else"; "true"; "false"; "def"; "PrintInt";
"PrintFloat"; "PrintString"; "Print"]
;;

let rec annotate_expr exp env : (aexpr * environment) = 
    match exp with
  | Unit        -> AUnit(TUnit), env
  | Literal(n)  -> ALiteral(n, TInt), env
  | FloatLit(n) -> AFloatLit(n, TFloat), env
  | BoolLit(n)  -> ABoolLit(n, TBool), env
  | ID(n)       -> if StringMap.mem n env 
                   then AID(n, StringMap.find n env), env
                   else raise Not_found
  | Binop(e1, op, e2) -> 
    let ae1, _ = annotate_expr e1 env 
    and ae2, _ = annotate_expr e2 env
    and ntyp = new_type () in
    ABinop(ae1, op, ae2, ntyp), env
  | Preop(preop, e) -> 
    let ae, _ = annotate_expr e env
    and ntyp = new_type () in
    APreop(preop, ae, ntyp), env
  | Postop(e, postop) ->
    let ae, _ = annotate_expr e env
    and ntyp = new_type () in 
    APostop(ae, postop, ntyp), env
(*  |  Assign(name, e) ->
    if StringMap.mem name env
    then raise (Failure "Reassignment")
    else let ntyp = new_type () in 
      StringMap.add name ntyp env in
    let ae = annotate_expr e env in 
    AAssign(name, e, ntyp) *)
  | List(e_list) ->
    let ae_list = List.map (fun e -> fst (annotate_expr e env)) e_list in 
    AList(ae_list, TList(new_type ())), env
  | Call(func, args) ->
    let a_func, _ = annotate_expr func env in
    let a_args = List.map (fun arg -> fst (annotate_expr arg env)) args in
    ACall(a_func, a_args, new_type ()), env
  | If(pred, e1, e2) ->
    let apred, _ = annotate_expr pred env 
    and e1, _ = annotate_expr e1 env
    and e2, _ = annotate_expr e2 env in
    AIf(apred, e1, e2, new_type ()), env
(*  | Fun(name, formals, e) ->
    (* TODO: Check for keywords being passed as args.
     * Currently, only takes first argument of function. 
     * So only test with one argument functions. *)
    if StringMap.mem name env 
    then raise (Failure "Redefining function") 
    else let nenv = StringMap.add name env in
    let nnenv = List.fold_left StringMap.add 
    let ae, _ = annotate_expr e env 
    and t = List.map (fun term -> StringMap.find term env) formals in 
    AFun(name, formals, ae, TFun(t, new_type())), nenv *)
  | _ -> AUnit(TUnit), env
;;

let type_of ae = 
    match ae with
  | AUnit(t)        -> t
  | ALiteral(_,t)   -> t
  | AFloatLit(_,t)  -> t
  | ABoolLit(_,t)   -> t
  | AID(_,t)        -> t
  | ABinop(_,_,_,t) -> t
  | APreop(_,_,t)   -> t
  | APostop(_,_,t)  -> t
  | AAssign(_,_,t)  -> t
  | ACall(_,_,t)    -> t
  | AFun(_,_,_,t)   -> t
  | AIf(_,_,_,t)    -> t
  | _               -> print_string "[Missed a type in type_of]"; TUnit
;;

let rec collect_expr ae : substitutions = 
    match ae with
  | ALiteral(_)     -> []
  | ABoolLit(_)     -> []
  | AFloatLit(_)    -> []
  | AString(_)      -> []
  | AUnit(_)        -> []
  | AID(_)          -> []
  | ABinop(ae1, op, ae2, t) ->
     let t1 = type_of ae1
     and t2 = type_of ae2 in
     let con = match op with 
       | Add | Sub | Mult | Div -> [(t1, TInt); (t2, TInt); (t, TInt)]
       | FAdd | FSub | FMult | FDiv ->
         [(t1, TFloat); (t2, TFloat); (t, TFloat)]
       | Neq | Equal | Greater | Less | Geq | Leq ->
         [(t1, t2); (t, TBool)]
       | And | Or -> [(t1, TBool); (t2, TBool); (t, TBool)] 
     in 
     (collect_expr ae1) @ (collect_expr ae2) @ con 
(*  | AAssign(name, e, t) ->  *)
  | AIf(pred, ae1, ae2, t) ->
    let pt = type_of pred and t1 = type_of ae1 and t2 = type_of ae2 in
    let con = [(pt, TBool); (t1, t2); (t, t1)] in 
    (collect_expr pred) @ (collect_expr ae1) @ (collect_expr ae2) @ con

  | AFun(_,_,ae,t) -> begin match t with 
    | TFun(_,ret_t) -> (collect_expr ae) @ [(type_of ae, ret_t)]
    | _ -> raise (Failure "Unreachable state in Function literal") end

  | ACall(name, args, t) -> 
          let fnt = (match name with 
            | AID(_) -> type_of name
            | _ -> raise (Failure "Unreachable state in Call") ) in 
          let s = match fnt with 
            | TFun(args_t, ret_t) -> begin
              let l1 = List.length args and l2 = List.length args_t in
              if l1 <> l2 then raise (Failure "Mismatched argument count")
              else let args_c = List.map2 (fun ft at -> (ft, type_of at)) args_t args in
                args_c @ [(t, ret_t)]
            end
            | TType(_) -> [(fnt, TFun(List.map type_of args, t))] 
            | _ -> raise (Failure "Mismatched types") in 
          (collect_expr name) @ (List.flatten (List.map collect_expr args)) @ s

  | e -> raise (Failure ("collect_expr can't handle your expr yet" ^ string_of_aexpr e) )
;;

let rec substitute u x t = 
    match t with 
    | TUnit | TInt | TBool | TFloat | TPitch | TString -> t
    | TType(c) -> if c = x then u else t
    | TFun(t1, t2) -> TFun(List.map (substitute u x) t1, substitute u x t2)
    | TList(t) -> TList(substitute u x t)
;;

let apply subs t = 
    List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

let rec unify constraints = 
    match constraints with 
  | [] -> []
  | (x, y) :: tl -> 
    let t2 = unify tl in 
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2

and unify_one t1 t2 =
    match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString | TUnit, TUnit | TFloat, TFloat | TPitch, TPitch -> []
  | TType(x), z | z, TType(x) -> [(x, z)] (* Not completely correct *)
  | TList(t1), TList(t2) -> unify_one t1 t2
  | TFun(u, v), TFun(x, y) ->
     let l1 = List.length u and l2 = List.length x in
     if l1 = l2 then unify ((List.combine u x) @ [(v, y)])  (* Double check if
         args are correct *)
     else raise (Failure "Mismatched Argument Count") 
  | _ -> raise (Failure "Mismatched types")
;;

let rec apply_expr subs ae = 
  match ae with
  | ALiteral(value, t)      -> ALiteral(value, apply subs t)
  | AFloatLit(value, t)     -> AFloatLit(value, apply subs t)
  | ABoolLit(value, t)      -> ABoolLit(value, apply subs t)
  | AID(name, t)            -> AID(name, apply subs t)
(*  |  AAssign(name, ae, t)    -> AAssign(name, apply_expr subs ae, apply subs
  t) *)
  | AFun(name, frmls, ae, t) 
                            -> AFun(name, frmls, apply_expr subs ae, apply subs t)
  | AIf(pred, ae1, ae2, t)  -> AIf(apply_expr subs pred, apply_expr subs ae1, 
                                    apply_expr subs ae2, apply subs t)
  | ACall(fname, args, t)   -> ACall(apply_expr subs fname, List.map (apply_expr subs) 
                                        args, apply subs t)
  | _ -> AUnit(TUnit) 
;;

let infer expr env = 
  let aexpr, nenv = annotate_expr expr env in
  let constraints = print_string ("[Annotated Expr:]" ^ string_of_aexpr aexpr); collect_expr aexpr in
  let subs = 
      List.iter (fun (a,b) -> print_string ("[Constraints:]" ^ string_of_typ a ^ string_of_typ b)) constraints; unify constraints in 
  let inferred_expr = apply_expr subs aexpr in 
  inferred_expr, nenv
;;

let typecheck program : (aexpr list) = 
  let env = Lib.predefined in 
  let inferred_program, _ = ListLabels.fold_left program ~init: ([], env) 
  
  ~f: (
        fun (acc, env) expr -> 

        let inferred_expr, env = 
          infer expr env in
        (inferred_expr :: acc, env)
      ) in
    List.rev inferred_program
;;

