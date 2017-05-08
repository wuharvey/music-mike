(* Semantic checking for the MicroC compiler *)

open Ast

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
  poly


