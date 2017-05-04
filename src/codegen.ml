(*
 Code generation: translate takes a semantically checked AST and
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


   let first  (a,_,_) = a;; 
    let second (_,a,_) = a;; 
    let third  (_,_,a) = a;; 

let main_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100
let function_defs:(string, (L.llvalue * A.expr)) Hashtbl.t = Hashtbl.create 100

(* , functions, structs *)
let translate (exprs) =
  let context = L.global_context () in
  
  let names:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 in
  let the_module = L.create_module  context "MusicMike"

  and i32_t   = L.i32_type          context      (* integer *)
  and i8_t    = L.i8_type           context      (* char? *) 
  and i1_t    = L.i1_type           context      (* boole *) 
  and float_t = L.double_type       context      (* float *)
  and void_t  = L.void_type         context in   (* void *)
  let i8p_t   = L.pointer_type i8_t   in         (* char pointer-string*) 
  let i32p_t  = L.pointer_type i32_t in          (* int* *)
  let i32pp_t = L.pointer_type i32p_t in         (* int**  *)
  let i32ppp_t= L.pointer_type i32pp_t in        (* int***  *)
  let floatp_t= L.pointer_type float_t in        (* float* *)
  let list_t  = L.named_struct_type context "list_struct" in 
  L.struct_set_body list_t  [| i32_t ; i32p_t |] true; 
  let listp_t = L.pointer_type list_t in 
(*   print_endline (string_of_bool (L.is_packed list_t)); *)

  let ltype_of_typ = function
      A.TInt     -> i32_t
    | A.TBool    -> i1_t
    (* | A.TVoid    -> void_t  *)
    | A.TFloat   -> float_t 
    | A.TString  -> i8p_t 
    | A.TUnit    -> void_t in


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

(*   let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.ident
      (* and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) *)
      in let ftype = L.function_type i32_t [||] in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
 *)

  let default_fun = L.define_function "main" (L.function_type (ltype_of_typ A.TInt) [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block default_fun) in


  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format = L.build_global_stringptr "%s\n" "str" builder in
  let float_format = L.build_global_stringptr "%f\n" "flt" builder in 
  let char_no_line = L.build_global_stringptr "%c" "str" builder in 
  let int_no_line = L.build_global_stringptr "%d " "fmt" builder in 

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
        let s_list = L.build_alloca list_t "array_struct" builder in
          let pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
            ignore(L.build_store (L.const_int i32_t (List.length es)) pointer builder);
          let arr_alloc = L.build_array_alloca (i32_t) (L.const_int i32_t (List.length es)) "array" builder
          in 
            let deal_with_element index e =  
              let pointer = L.build_gep arr_alloc [| (L.const_int i32_t index)|] "elem" builder in 
              let e' = expr builder e in 
                ignore(L.build_store e' pointer builder)
            in
             List.iteri deal_with_element es;
          let pointer_arr = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 1 |] "actual_list" builder in 
            ignore(L.build_store arr_alloc pointer_arr builder);
        s_list
    | A.Subset(s, index)  ->
          let var = try Hashtbl.find main_vars s
                    with Not_found -> raise (Failure (s ^ " Not Found!"))
            in 
              let head = L.build_load var "head" builder in 
              let pointer = L.build_gep head [| (L.const_int i32_t index) |] "pointer" builder in 
               L.build_load pointer "tmp" builder
  
    | A.RList(es) ->
          let arr_malloc = L.build_array_malloc (float_t) (L.const_int i32_t (List.length es)) "array" builder
          in 
            let deal_with_element index e =  
              let pointer = L.build_gep arr_malloc [| (L.const_int i32_t index)|] "elem" builder in 
              let e' = expr builder e in 
                ignore(L.build_store e' pointer builder)
            in
             List.iteri deal_with_element es; arr_malloc

	    
    | A.ChordList(cs) ->
      (* allocates the chord list *)
	    let arr_malloc = L.build_array_malloc (i32pp_t) (L.const_int i32_t (List.length cs)) "chord_pointer_array" builder in
	    
              let iter_thru_chord index chord=
  		(* assigns pointer to chord *)
  		let chord_pointer = L.build_gep arr_malloc [| (L.const_int i32_t index)|] "chord_pointer_elem" builder in
  		(*allocates  array for pitches*)
  		let arr_chord_malloc = L.build_array_malloc (i32p_t) (L.const_int i32_t (List.length chord)) "arr_pitch" builder in
                  (* stores array for pitches into pointer to chord*)	
                  ignore(L.build_store arr_chord_malloc chord_pointer builder); 

		  let deal_with_pitch index el=
		    (*assigns a pointer to the pitch *)
		    let pitch_pointer = L.build_gep arr_chord_malloc [| (L.const_int i32_t index)|] "pitch_pointer_elem" builder in
		    (* allocates single pitch *)
		    let arr_pitch_malloc = L.build_array_malloc (i32_t) (L.const_int i32_t (3)) "pitch" builder in  
		    (* stores allocated pitch into pointer for the pitch  *)
		    ignore(L.build_store arr_pitch_malloc pitch_pointer builder);
                	(* for each field of pitch tuple, allocate space and write in *)
			(* prefield *)
			let prefield_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 0)|] "prefield_elem" builder in
			let el'=L.const_int i32_t (first el)  in
			ignore(L.build_store el' prefield_pointer builder);
			(*scale degree *)
                        let sd_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 1)|] "scaledegreer_elem" builder in
                        let el'=expr builder  (second el) in
                        ignore(L.build_store el' sd_pointer builder); 
			(*posfield*)
                        let postfield_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 2)|] "postfield_elem" builder in
                        let el'=L.const_int i32_t (third el) in
                        ignore(L.build_store el' postfield_pointer builder); 
  		in
                  (* iterates through pitches with deal_with_pitch*)
  		ignore(List.iteri deal_with_pitch chord)
             in
             (*iterates through chords with iter_thru_chord *)   
             ignore(List.iteri iter_thru_chord cs); arr_malloc


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
                          A.List(_) -> listp_t
		                    | A.RList(_) -> floatp_t 
                        | _ -> i32_t) s builder in 
                        Hashtbl.add main_vars s local_var;local_var in
                ignore (L.build_store e' var builder); e' 


    | A.Call (A.ID("Printint"), [e]) ->
       L.build_call printf_func [| int_format_str ; (expr builder e) |]  "printf" builder
    | A.Call (A.ID("Printstr"), [e]) ->
       L.build_call printf_func [| str_format; (expr builder e) |] "printf" builder
    | A.Call (A.ID("Printfloat"), [e]) ->
       L.build_call printf_func [| float_format; (expr builder e) |] "printf" builder
    | A.Call (A.ID("Printlist"), [e]) -> 
      let s_list = expr builder e in 
        let pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
          let length = L.build_load pointer "size" builder in 
            let list_pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 1 |] "cur_list_ptr" builder in 
            let act_list = L.build_load list_pointer "cur_list" builder in
            let cur_index_ptr = L.build_alloca i32_t "cur_index_ptr" builder in 
            let cur_index = L.build_store (L.const_int i32_t 0) cur_index_ptr builder in 
            
              (* L.build_call printf_func [|int_format_str ; pred |] "printf" builder *)

            let cur_fun = L.block_parent (L.insertion_block builder) in 
            let pred_bb = L.append_block context "while" cur_fun in 
              ignore (L.build_br pred_bb builder);

            let body_bb = L.append_block context "while_body" cur_fun in let body_builder = L.builder_at_end context body_bb in
                let ptr_to_idx = L.build_in_bounds_gep act_list [| L.build_load cur_index_ptr "cur_indexplz" body_builder |] "cur_val" body_builder in
                  let val_idx = L.build_load ptr_to_idx "val_idx" body_builder in  
              ignore(L.build_call printf_func [|int_no_line ; val_idx |] "printf" body_builder);
              let cur_index_val = L.build_load cur_index_ptr "cur_index" body_builder in
              let new_idx = L.build_add cur_index_val (L.const_int i32_t 1) "new_idx" body_builder in 
                ignore(L.build_store new_idx cur_index_ptr body_builder);
                ignore(L.build_br pred_bb body_builder); 
            let pred_builder = L.builder_at_end context pred_bb in
            let cur_index_val2 = L.build_load cur_index_ptr "cur_index2" pred_builder in
            let bool_val = L.build_icmp L.Icmp.Ne length cur_index_val2 "pred" pred_builder in 

            let merge_bb = L.append_block context "merge" cur_fun in 
              ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
              L.position_at_end merge_bb builder;
              L.const_int i32_t 1;


    | A.Call (A.ID(s), act) ->
       let (fdef, fdecl) = Hashtbl.find function_defs s  in
       let actuals = List.rev (List.map (expr builder) (List.rev act)) in
(* let result = (match fdecl.A.typ with A.Void -> ""
    | _ -> f ^ "_result") in *)
       L.build_call fdef (Array.of_list actuals) "" builder

    | A.If(e1, e2, e3) -> 
        let bool_val = expr builder e1 in 
          let cur_fun = L.block_parent (L.insertion_block builder) in 
          let merge_bb = L.append_block context "merge" cur_fun in

          let then_bb = L.append_block context "then" cur_fun in
          ignore(expr (L.builder_at_end context then_bb) e2); ignore(L.build_br merge_bb (L.builder_at_end context then_bb));

          let else_bb = L.append_block context "else" cur_fun in 
          ignore(expr (L.builder_at_end context else_bb) e3); ignore(L.build_br merge_bb (L.builder_at_end context else_bb));

          ignore(L.build_cond_br bool_val then_bb else_bb builder); 
          L.position_at_end merge_bb builder;
            L.const_int i32_t 1 
    | A.Fun(fid, arg_list, e) -> 
      (* and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) *)
      let ftype = L.function_type i32_t [||] in
      let the_function = L.define_function fid ftype the_module in
      Hashtbl.add function_defs fid (the_function, A.Fun(fid, arg_list, e)); 
      let builder2 = L.builder_at_end context (L.entry_block the_function) in 
      let ret_val = expr builder2 e in 
        L.build_ret ret_val builder2


    | _ -> L.const_int i32_t 1
  in 
    let exprbuilder builder e = ignore(expr builder e); builder
  in
    let builder = List.fold_left exprbuilder builder (List.rev(exprs))

  in(* 
  let build_fun_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.A.ident function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in 
    let ret_val = expr builder fdecl.A.body in  
    ignore(L.build_ret ret_val builder)
  in
    List.iter build_fun_body functions; 
 *)
  ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module
