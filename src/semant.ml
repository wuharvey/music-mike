(* Semantic checking for the MicroC compiler *)

open Ast
open Infer
module StringMap = Map.Make(String)

(* Hack for polymorphism compilation: take each polymorphic function and check when it is called.
 * Create a new Function Aexpr for each time it is called, with the specific type of the actuals. *)

let check (aexprs: aexpr list) =
  let is_poly ae = match ae with
    | AFun(_,_,_,TFun(f_t, r_t)) ->
        let poly t = match t with
          | TType(_) -> true
          | _ -> false
        in
        List.exists poly f_t
    | _ -> false
  in
  let poly = List.filter is_poly aexprs in
  let getname ae = match ae with
    | AFun(fn,_,_,_) -> fn
    | _ -> raise (Failure "What the hell you doin")
  in
  let poly_fnames = List.map getname poly in

  let is_call ae = match ae with
    | ACall(AID(fn,t), args, _) -> let name = fn in
      List.mem name poly_fnames
    | _ -> false
  in
  let polycalls = List.filter is_call aexprs in
  let rec matching x lst =
      match lst
      with [] -> []
    | ACall(AID(x,t), a, b)::rest -> ACall(AID(x,t), a, b)::(matching x rest)
    | y::rest -> matching x rest
    in
  let rec iterAexprs alist =
    match alist with
    [] -> []
    | AFun(fn, a, b, _)::rest -> let callmatches = matching fn polycalls in
        let typelist cm =
          (match cm with
          ACall(_, c, _) -> List.map Infer.type_of c
          | _ -> [] )
        in
        let calltofun cm1 = AFun(fn, a, b, TFun(typelist cm1, Infer.type_of cm1)) in
        (List.map calltofun callmatches)@(iterAexprs rest)
    | w::rest -> w::(iterAexprs rest)
  in iterAexprs aexprs;;
