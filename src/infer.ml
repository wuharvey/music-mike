open Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
type environment = typ StringMap.t
type constraints = (typ * typ) list 
(* TODO: Right now everything is a global *)

let letter = ref (Char.code 'a');;

let new_type () = let c1 = !letter in
  incr letter; TType(Char.escaped (Char.chr c1))
;;
   
let kws = ["if"; "then"; "else"; "true"; "false"; "def"]
;;

let keywords = 
  List.fold_left (fun set x -> StringSet.add x set) StringSet.empty kws
;;

let rec annotate_expr exp env : (aexpr * environment) = 
  match exp with
  | Unit        -> AUnit(TUnit), env
  | Literal(n)  -> ALiteral(n, TInt), env
  | FloatLit(n) -> AFloatLit(n, TFloat), env
  | BoolLit(n)  -> ABoolLit(n, TBool), env
  | String(n)   -> AString(n, TString), env
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
  | Assign(name, e) -> 
    if StringMap.mem name env
      then raise (Failure "Reassignment")
      else if StringSet.mem name keywords
      then raise (Failure "Redefining keyword")
      else let ntyp = new_type () in 
    let nenv = StringMap.add name ntyp env in
    let ae, _ = annotate_expr e nenv in 
    AAssign(name, ae, ntyp), nenv 
  | List(e_list) ->
    let ae_list = List.map (fun e -> fst (annotate_expr e env)) e_list in 
    AList(ae_list, TList(new_type ())), env
  | RList(e_list) -> 
    let ae_list = List.map (fun e -> fst (annotate_expr e env)) e_list in
    AList(ae_list, TList(TFloat)), env
  | Call(func, args) ->
    let a_func, _ = annotate_expr func env in
    let a_args = List.map (fun arg -> fst (annotate_expr arg env)) args in
    ACall(a_func, a_args, new_type ()), env
  | If(pred, e1, e2) ->
    let apred, _ = annotate_expr pred env 
    and e1, _ = annotate_expr e1 env
    and e2, _ = annotate_expr e2 env in
    AIf(apred, e1, e2, new_type ()), env
  | Fun(name, args, e) ->
    let args_t = List.map (fun f -> new_type ()) args
    and ret_t = new_type() in 
    let fun_t = TFun(args_t, ret_t) in
      let a_args = List.combine args args_t in 
      let nenv = List.fold_left  
            (fun e (id, t) -> 
              if StringMap.mem id e
              then raise (Failure "Variable already defined")
              else StringMap.add id t e) env a_args 
      in
    if StringMap.mem name env 
    then raise (Failure "Redefining function") 
    else let nenv = StringMap.add name fun_t nenv in
    let ae, _ = annotate_expr e nenv in 
    AFun(name, args, ae, fun_t), nenv  
  | _ -> AUnit(TUnit), env
;;

let type_of ae = 
  match ae with
  | AUnit(t)        -> t
  | ALiteral(_,t)   -> t
  | AFloatLit(_,t)  -> t
  | AString(_,t)    -> t
  | ABoolLit(_,t)   -> t
  | AID(_,t)        -> t
  | ABinop(_,_,_,t) -> t
  | APreop(_,_,t)   -> t
  | APostop(_,_,t)  -> t
  | AAssign(_,_,t)  -> t
  | ACall(_,_,t)    -> t
  | AFun(_,_,_,t)   -> t
  | AList(_,t)      -> t
  | AIf(_,_,_,t)    -> t
  | _               -> print_string "[Missed a type in type_of]"; TUnit
;;


let rec collect_expr ae : constraints = 
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
      | FAdd | FSub | FMult | FDiv -> [(t1, TFloat); (t2, TFloat); (t, TFloat)]
      | Neq | Equal | Greater | Less | Geq | Leq -> [(t1, t2); (t, TBool)]
      | And | Or -> [(t1, TBool); (t2, TBool); (t, TBool)] 
    in 
    (collect_expr ae1) @ (collect_expr ae2) @ con 

  | AAssign(_, ae, t) -> (collect_expr ae) @ [(t, type_of ae)]
  | AList(ae_list, t)   ->
    let list_t = match t with
      | TList(s) -> s
      | _ -> raise (Failure "Unreachable state in List literal") in
    let con = List.map (fun aexpr -> (list_t, type_of aexpr)) ae_list in 
    (List.flatten (List.map collect_expr ae_list)) @ con
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
      | TFun(args_t, ret_t) -> 
        begin
          let l1 = List.length args and l2 = List.length args_t in
          if l1 <> l2 
          then raise (Failure "Mismatched argument count")
          else let args_c = List.map2 (fun ft at -> (ft, type_of at)) args_t args in
          args_c @ [(t, ret_t)]
        end
      | TType(_) -> [(fnt, TFun(List.map type_of args, t))] 
      | _ -> raise (Failure "Mismatched types")
    in 
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
  | TInt, TInt | TBool, TBool | TString, TString 
  | TUnit, TUnit | TFloat, TFloat | TPitch, TPitch -> []
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
  | ALiteral(value, t)       -> ALiteral(value, apply subs t)
  | AFloatLit(value, t)      -> AFloatLit(value, apply subs t)
  | ABoolLit(value, t)       -> ABoolLit(value, apply subs t)
  | AString(value, t)        -> AString(value, apply subs t)
  | ABinop(ae1, op, ae2, t)  -> 
      ABinop(apply_expr subs ae1, op, apply_expr subs ae2, apply subs t) 
  | AID(name, t)             -> AID(name, apply subs t)
  | AAssign(name, ae, t)     -> 
      AAssign(name, apply_expr subs ae, apply subs t) 
  | AList(ae_list, t)        -> 
      AList(List.map (apply_expr subs) ae_list, apply subs t)
  | AFun(name, frmls, ae, t) -> 
      AFun(name, frmls, apply_expr subs ae, apply subs t)
  | AIf(pred, ae1, ae2, t)   -> 
      AIf(apply_expr subs pred, apply_expr subs ae1, 
          apply_expr subs ae2, apply subs t)
  | ACall(fname, args, t)    -> 
      ACall(apply_expr subs fname, 
            List.map (apply_expr subs) args, apply subs t)
  | e -> raise (Failure ("No apply_expr for AEXPR:" ^ string_of_aexpr e))  
;;

let infer expr env flag = 
  let aexpr, nenv = annotate_expr expr env in
  let constraints =
    if flag
    then print_endline ("AEXPR: " ^ string_of_aexpr aexpr);
    collect_expr aexpr in
  let subs = 
    List.iter (fun (a,b) -> 
      if flag then
      print_endline 
      ("CONSTRAINTS: " ^ string_of_typ a ^ " "  ^ string_of_typ b)) constraints;
      unify constraints in 
  let inferred_expr =
      List.iter (fun (a,b) -> if flag then
        print_endline ("SUBS: " ^ a ^ " "  ^ string_of_typ b)) subs;  
        apply_expr subs aexpr in 
        if flag then
        print_endline("FINAL: " ^ string_of_aexpr inferred_expr);
  inferred_expr, nenv
;;

let typecheck program flag : (aexpr list) = 
  let env = Lib.predefined in 
  let inferred_program, _ = ListLabels.fold_left (List.rev program) 
  
  ~init: ([], env) 
  
  ~f: (
        fun (acc, env) expr -> 
        let inferred_expr, env = infer expr env flag in
        let inferred_expr, env = match inferred_expr with
          | AAssign(name, _, t) -> 
            let env = StringMap.add name t env in
            inferred_expr, env
          | AFun(name, _, _, t) ->
            let env = StringMap.add name t env in
            inferred_expr, env
          | _ -> inferred_expr, env in  
        (inferred_expr :: acc, env)
      ) 
    
    in List.rev inferred_program
;;

