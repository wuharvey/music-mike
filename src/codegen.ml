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

(*
(*define string hash *)

module StringHash=Hashtbl.Make(struct
	type t=string       (* type of key *)
	let equal x y = x = y    (*use structural comparison *)
 	let hash = Hash

)
*)
let main_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100
let function_defs:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100


let translate (exprs, functions, structs) =
  let context = L.global_context () in
  
  let names:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
  let the_module = L.create_module  context "MusicMike"
  and i32_t   = L.i32_type          context      (* integer *)
  and i8_t    = L.i8_type           context      (* char? *) 
  and i1_t    = L.i1_type           context      (* boole *) 
  and float_t = L.double_type       context      (* float *)
  and void_t  = L.void_type         context in   (* void *)
  let i8p_t   = L.pointer_type i8_t   in         (* char pointer-string*) 
  let i32p_t  = L.pointer_type i32_t in          (* int pointer *)

  let ltype_of_typ = function
      A.Int     -> i32_t
    | A.Bool    -> i1_t
    | A.Void    -> void_t 
    | A.Float   -> float_t 
    | A.String  -> i8p_t in 
                        

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in


(* 
  let local_main_vars = StringMap.empty in 
  let print_var s llv = print_string(s ^ ": " ^ L.string_of_llvalue llv ^ "\n") in *)
  
(*   let process_fun_decl fdecl = 
    let name = fdecl.A.name in 
      print_string("hello")
  in *)

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.ident
      (* and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) *)
      in let ftype = L.function_type i32_t [||] in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  let default_fun = L.define_function "main" (L.function_type (ltype_of_typ A.Int) [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block default_fun) in


  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format = L.build_global_stringptr "%s\n" "str" builder in
  let float_format = L.build_global_stringptr "%f\n" "flt" builder in 
  let rec expr builder = function
      A.Literal(i) ->  L.const_int i32_t i
    | A.FloatLit f -> L.const_float float_t f 
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Noexpr -> L.const_int i32_t 0
    | A.ID s -> L.build_load (try Hashtbl.find main_vars s with Not_found -> raise(Failure(s ^ " Not Found"))) s builder
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
    | A.List(es)    -> 
          let arr_malloc = L.build_array_malloc (i32_t) (L.const_int i32_t (List.length es)) "array" builder
          in 
            let deal_with_element index e =  
              let pointer = L.build_gep arr_malloc [| (L.const_int i32_t index)|] "elem" builder in 
              let e' = expr builder e in 
                ignore(L.build_store e' pointer builder)
            in
             List.iteri deal_with_element es; arr_malloc
    | A.Subset(s, index)  ->
          let var = try Hashtbl.find main_vars s
                    with Not_found -> raise (Failure (s ^ " Not Found!"))
            in 
              let head = L.build_load var "head" builder in 
              let pointer = L.build_gep head [| (L.const_int i32_t index) |] "pointer" builder in 
               L.build_load pointer "tmp" builder
    | A.PList(mod_plist) -> 
(*	let mod_plist=     				(*modified version of pitch list where each pitch is list of 3 intsi*)
	  let deal_with_pitch =function                 (*decomposes pitch list making up chord and shrinks pitch *) 
             [] -> []
           | head :: tail ->
		head=List.fold_left (fun s e -> s+e) 0 fst head :: fst scnd head :: [List.fold_left (fun s e -> s+e) 0 third head]; 
							(*sums first field of pitch tuple, gets first element of second field (1st element of list length 1) and sums third field of tuple *)

		deal_with_pitch tail 
          in List.iter deal_with_pitch cs  in            (*List.iter takes care of chords----at this point mod_plist is defined*)
*)
        let num_of_pointers = List.length cs
        in  

        (*first layer pointers-----allocates space for array of pointers to chords *)
        let arr_malloc=L.build_array_malloc (i32p_t) (L.const_int i32_t (num_of_pointers)) "chord_pointer_array" builder in
        let deal_with_chord index chord=          (* List.iter will be applied so e represents a chord *)
        let chord_pointer = L.build_gep arr_malloc [| (L.const_int i32_t index)|] "chord_pointer_elem" builder in

                (*second layer pointers-----allocates spaces for array of pointers to pitches *)
                let arr_list_pitch = L.build_array_malloc (i32p_t) (L.const_int i32_t (List.length e)) "pitch_pointer_array" builder  in
                let deal_with_pitch index1 e_pitch=
                let pointer_p=L.build_gep arr_malloc [| (L.const_int i32_t index1)|] "pitch_pointer_elem" builder in

                        (*third layer pointers-----allocates space for indevidual pitch (list of 3 fields *)
                        let arr_pitch = L.build_array_malloc (i32_t) (L.const_int i32_t (3)) "pitch_array" builder  in
                        let deal_with_pfield index2 e_field=
                        let pointer_f = L.build_gep arr_malloc [| (L.const_int i32_t index2)|] "pfield_elem" builder in
                        let e_field'=i32_t(e_field) in  (*converts int into llvm int *)
                        ignore(L.build_store e_field' pointer_f builder) in
                        List.iteri deal_with_pfield e_pitch 
                in
                ignore(L.build_store arr_pitch pointer_p builder) in
		List.iteri deal_with_pitch chord 
        in
        ignore(L.build_store arr_list_pitch chord_pointer builder) in
        List.iteri deal_with_chord mod_plist; arr_malloc  








    | A.Block(es) -> 
        (match es with 
        e::e1::rest -> ignore(expr builder e); expr builder (A.Block(e1::rest))
      | [e] -> expr builder e)
    | A.Preop(op, e) ->
       let e' = expr builder e in
       (match op with
		  A.Neg     -> L.build_neg
		| A.Not     -> L.build_not
       ) e' "tmp" builder
    | A.Assign (s, e) -> let e' = expr builder e in 
          let var = try Hashtbl.find main_vars s 
                    with Not_found ->  
                    let local_var = L.build_alloca (match e with 
                         A.List(_) -> i32p_t 
                        | _ -> i32_t) s builder in 
                        Hashtbl.add main_vars s local_var;local_var in
                ignore (L.build_store e' var builder); e' 


    | A.Call ("Printint", [e]) ->
       L.build_call printf_func [| int_format_str ; (expr builder e) |]  "printf" builder
    | A.Call ("Printstr", [e]) ->
       L.build_call printf_func [| str_format; (expr builder e) |] "printf" builder
    | A.Call ("Printfloat", [e]) ->
       L.build_call printf_func [| float_format; (expr builder e) |] "printf" builder
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
    let builder = List.fold_left exprbuilder builder (List.rev(exprs))

  in
  let build_fun_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.A.ident function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in 
    let ret_val = expr builder fdecl.A.body in  
    ignore(L.build_ret ret_val builder)
  in
    List.iter build_fun_body functions; 

  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module
