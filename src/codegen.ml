(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (exprs, functions, structs) =
  let context = L.global_context () in
  
  let names:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
  let the_module = L.create_module  context "MusicMike"
  and i32_t   = L.i32_type          context
  and i8_t    = L.i8_type           context
  and i1_t    = L.i1_type           context
  and float_t = L.float_type        context 
  and void_t  = L.void_type         context in 
  let i8p_t   = L.pointer_type i8_t   in

 
  let ltype_of_typ = function
      A.Int     -> i32_t
    | A.Bool    -> i1_t
    | A.Void    -> void_t 
    | A.Float   -> float_t 
    | A.String  -> i8p_t in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let default_fun = L.define_function "main" (L.function_type (ltype_of_typ A.Void) [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block default_fun) in
  
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format = L.build_global_stringptr "%s\n" "str" builder in

  let rec expr builder = function
      A.Literal i ->  L.const_int i32_t i
    | A.FloatLit f -> L.const_float float_t f 
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    (* | A.ID s -> L.build_load (lookup s) s builder *)
    | A.String s -> L.build_global_stringptr s "" builder
    | A.Binop (e1, op, e2) ->
            let e1' = expr builder e1
    and e2' = expr builder e2 in
            (match op with
      A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.FAdd    -> L.build_fadd
    | A.FSub    -> L.build_fsub
    | A.FMult   -> L.build_fmul
    | A.FDiv    -> L.build_fdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Less    -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    ) e1' e2' "tmp" builder
 (* | A.Unop(op, e) ->
       let e' = expr builder e in
       (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
    | A.Assign (s, e) -> let e' = expr builder e in
    ignore (L.build_store e' (lookup s) builder); e' *)
    | A.Call ("Printint", [e]) ->
       L.build_call printf_func [| int_format_str ; (expr builder e) |]  "printf" builder
    | A.Call ("Printstr", [e]) ->
       L.build_call printf_func [| str_format; (expr builder e) |] "printf" builder
           (* | A.Call (f, act) ->
               let (fdef, fdecl) = StringMap.find f function_decls in
  let actuals = List.rev (List.map (expr builder) (List.rev act)) in
  let result = (match fdecl.A.typ with A.Void -> ""
            | _ -> f ^ "_result") in
  L.build_call fdef (Array.of_list actuals) result builder *)
    | _ -> L.const_int i32_t 1
  in 
    let exprbuilder builder e = ignore(expr builder e); builder
  in
    let builder = List.fold_left exprbuilder builder exprs





  in
  ignore (L.build_ret_void builder);
  the_module
