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
module I = Infer

module StringMap = Map.Make(String)


   let first  (a,_,_) = a;; 
    let second (_,a,_) = a;; 
    let third  (_,_,a) = a;; 

let main_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100
let function_defs:(string, (L.llvalue * A.aexpr)) Hashtbl.t = Hashtbl.create 100

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

  (* int list struct  *)
  let list_t  = L.named_struct_type context "list_struct" in 
  L.struct_set_body list_t  [| i32_t ; i32p_t |] true; 
  let listp_t = L.pointer_type list_t in 

  (* int * list struct *) 
  let chord_struct  = L.named_struct_type context "chord_struct" in
  L.struct_set_body chord_struct  [| i32_t ; i32pp_t |] true;
  let chord_structp = L.pointer_type chord_struct in
 
  (* int ** list struct  *)
  let chordlist_struct  = L.named_struct_type context "chordlist_struct" in
  L.struct_set_body chordlist_struct  [| i32_t ; chord_structp |] true;
  let chordlist_structp = L.pointer_type chordlist_struct in
 


  (* float list struct   *)
  let list_t_f  = L.named_struct_type context "list_struct_f" in 
  L.struct_set_body list_t_f  [| i32_t ; floatp_t |] true; 
  let listp_t_f = L.pointer_type list_t_f in
 

  let ltype_of_typ = function
      A.TInt     -> i32_t
    | A.TBool    -> i1_t
    | A.TList(A.TInt)    -> listp_t
    | A.TList(A.TFloat)  -> listp_t_f
    | A.TList(A.TList(A.TPitch)) -> chordlist_structp
    (* | A.TVoid    -> void_t  *)
    | A.TFloat   -> float_t 
    | A.TString  -> i8p_t 
    | A.TUnit    -> void_t 

    | _ -> raise (Failure "Shouldn't be here") in
    
  let stype_of_typ = function 
    A.TList(A.TInt) -> (i32_t, list_t) 
  | A.TList(A.TFloat) -> (float_t, list_t_f)
  | _ -> raise (Failure "No Struct of this type") in


  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let default_fun = L.define_function "main" (L.function_type (ltype_of_typ A.TInt) [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block default_fun) in


  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let str_format = L.build_global_stringptr "%s\n" "str" builder in
  let float_format = L.build_global_stringptr "%f\n" "flt" builder in 
  let char_no_line = L.build_global_stringptr "%c" "str" builder in 
  let int_no_line = L.build_global_stringptr "%d " "fmt" builder in 
  let float_no_line = L.build_global_stringptr "%f " "fmt" builder in
  (* Declare the built-in synth() function *)
  let synth_t = L.function_type i32_t [|i32ppp_t ; i32_t ; i32p_t; i32_t; i32p_t; i32_t; floatp_t; i32pp_t  |] in
  let synth_func = L.declare_function "synth" synth_t the_module in



  (* get length of struct *)
  let get_length (struct_obj, sub_builder) = (* print_endline(L.string_of_lltype (L.type_of struct_obj)); *)
    (* get pointer to length in the struct (at position 0,0) *)
  let pointer = L.build_in_bounds_gep struct_obj [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" sub_builder in(*  print_endline(L.string_of_lltype (L.type_of pointer)); *)
          (* load that pointer to the length *)
          L.build_load pointer "size" sub_builder 
  in 
  let get_list (struct_obj, sub_builder) = 
    (* get pointer to the int* in the struct (at position 0,1) *)
    let list_pointer = L.build_in_bounds_gep struct_obj [| L.const_int i32_t 0; L.const_int i32_t 1 |] "cur_list_ptr" sub_builder in 
    (* load that pointer - now act_list is the pointer to the head of the list *)
    L.build_load list_pointer "cur_list" sub_builder 
  in


(* s_list is llvalue, application is function taking element of list, index, and builder *)
let map s_list  application = 
      (* cur_index = 0
          while cur_index < length:
            printf act_list[cur_index] 
            cur_index += 1*)
       (* get pointer to length in the struct (at position 0,0) *)
        let pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
          (* load that pointer to the length *)
          let length = L.build_load pointer "size" builder in 
            (* get pointer to the int* in the struct (at position 0,1) *)
            let list_pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 1 |] "cur_list_ptr" builder in 
            (* load that pointer - now act_list is the pointer to the head of the list *)
            let act_list = L.build_load list_pointer "cur_list" builder in
            (* allocate a pointer to an int (on the stack) *)
            let cur_index_ptr = L.build_alloca i32_t "cur_index_ptr" builder in 
            (* store a 0 in that location *)
            let cur_index = L.build_store (L.const_int i32_t 0) cur_index_ptr builder in 
            
            (* we are creating blocks, so we need the function we are currently in *)
            let cur_fun = L.block_parent (L.insertion_block builder) in 
            (* create the block that's supposed to have cur_index < length
               "the conditional block" ==> pred_bb *)
            let pred_bb = L.append_block context "while" cur_fun in 
              ignore (L.build_br pred_bb builder);

            (* create the block of the body - basically 
                printf act_list[cur_index] *)
            let body_bb = L.append_block context "while_body" cur_fun in 
              (* body_builder is the builder in the "while body" *)
              let body_builder = L.builder_at_end context body_bb in

            (* DO THE WORK ON THE ACTUAL ELEMENTS OF THE LIST HERE *)
                (* loads the value in cur_index_ptr *)
                let cur_idx_in_body = L.build_load cur_index_ptr "cur_indexplz" body_builder in
                (* get a pointer into the list at the index with the value just loaded *)
                let ptr_to_idx = L.build_in_bounds_gep act_list [| cur_idx_in_body |] "cur_val" body_builder in
                  (* load the value at that pointer (aka value of act_list[cur_index]) *)
                  let val_idx = L.build_load ptr_to_idx "val_idx" body_builder in 
               (* apply function onto element*)
              ignore(application  val_idx cur_idx_in_body body_builder);
(*               print_endline("line 169");
 *)            (* END WORK HERE *)
            
              (* loads the value in cur_index_ptr *)
              let cur_index_val = L.build_load cur_index_ptr "cur_index" body_builder in
              (* add 1 to the value *)
              let new_idx = L.build_add cur_index_val (L.const_int i32_t 1) "new_idx" body_builder in 
                (* store the new value in the pointer  *)
                ignore(L.build_store new_idx cur_index_ptr body_builder);
                ignore(L.build_br pred_bb body_builder); 

            (* the builder at the "check if cur_index < length" *)
            let pred_builder = L.builder_at_end context pred_bb in
            let cur_index_val2 = L.build_load cur_index_ptr "cur_index2" pred_builder in
            let bool_val = L.build_icmp L.Icmp.Ne length cur_index_val2 "pred" pred_builder in 

            let merge_bb = L.append_block context "merge" cur_fun in 
              ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
              L.position_at_end merge_bb builder;
         in

  let rec expr builder = function
      A.ALiteral(i, _) ->  L.const_int i32_t i
    | A.AFloatLit(f, _) -> L.const_float float_t f 
    | A.ABoolLit(b, _) -> L.const_int i1_t (if b then 1 else 0)
    | A.ANoexpr -> L.const_int i32_t 0
    | A.AID(s, _) -> L.build_load (try Hashtbl.find main_vars s with Not_found -> raise(Failure(s ^ " Not Found"))) s builder
    | A.AString(s, _) -> L.build_global_stringptr s "" builder
    | A.ABinop (e1, op, e2, _) ->
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

   | A.APitch(preop, e, postop, _) ->
			  (* allocates single pitch *)
        let arr_pitch_malloc = L.build_array_malloc i32_t (L.const_int i32_t 3) "array" builder in
				(* prefield *)
				let prefield_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 0)|] "prefield_elem" builder in
				let el'=L.const_int i32_t preop  in
				ignore(L.build_store el' prefield_pointer builder);
				(*scale degree *)
				let sd_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 1)|] "scaledegreer_elem" builder in
				let el'=expr builder e in
				ignore(L.build_store el' sd_pointer builder); 
				(*posfield*)
				let postfield_pointer=L.build_gep arr_pitch_malloc [| (L.const_int i32_t 2)|] "postfield_elem" builder in
				let el'=L.const_int i32_t postop in
				ignore(L.build_store el' postfield_pointer builder); 
        arr_pitch_malloc
        

   | A.AList(es, t)    ->
	    ( match t with
       | TList(TList(TPitch)) -> (*list_t --> i32_t*)
        (* struct_a = {i32_t, i32_t*}       <- list_t
           struct_b = {i32_t, struct_a* }     <- list_i32p_t
           struct_c = {i32_t, struct_b* }   <- list_i32pp_t *)
		    (*allocate struct to hold chordlist and length, struct_c {i32_t, struct_b*} *)
		    let cl_struct=L.build_malloc chordlist_struct "cl_struct" builder in
		      (* builds pointer for length field in struct to hold length of chordlist *)
          let c_len_pointer = L.build_in_bounds_gep cl_struct [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in 
              ignore(L.build_store (L.const_int i32_t (List.length es)) c_len_pointer builder);
          (* malloc's an array of type struct_b {i32_t, i32_t** } *)
		      let arr_malloc = L.build_array_malloc chord_struct (L.const_int i32_t (List.length es)) "chord_pointer_array" builder in 
          (*makes chord struct- length + content *)
		      (* iterates thru es and builds each chord *)  
           
		      let iter_thru_chord index chord =
      			 let cl2c_pointer = L.build_gep arr_malloc [| (L.const_int i32_t index)|] "pointer_chord_elem_list" builder in
             let e' = expr builder chord in 
              let e_val = L.build_load e' "actual_chord_struct" builder in 
      				ignore(L.build_store e_val  cl2c_pointer builder);
      		  in
  		     (*iterates through chords with iter_thru_chord *)   
  		     List.iteri iter_thru_chord es;
           (* make pointer to chord array in s list *)
          let cl_pointer_arr = L.build_in_bounds_gep cl_struct [| L.const_int i32_t 0; L.const_int i32_t 1 |] "struct_cl_pointer" builder in 
          (* fill arr_malloc into pointer to chord list struct *)
          ignore(L.build_store arr_malloc cl_pointer_arr builder); cl_struct
  
       |  TList(TPitch) -> (*A List of Pitches (aka things separated by | ) *)
	       (* allocates the chord list *)
    			let c_struct=L.build_malloc chord_struct "chord_struct" builder in
    			 let c_len_pointer = L.build_in_bounds_gep c_struct [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
    				ignore(L.build_store (L.const_int i32_t (List.length es)) c_len_pointer builder);
    			 let arr_chord_malloc = L.build_array_malloc i32p_t (L.const_int i32_t (List.length es)) "arr_pitch" builder in
    			 (* ties c_struct to cl_struct  *)
      			let deal_with_pitch index el = 
      			    (*assigns a pointer to the pitch *)
      			    let pitch_pointer = L.build_gep arr_chord_malloc [| (L.const_int i32_t index)|] "pitch_pointer_elem" builder in
                  let e' = expr builder el in 
(*                   let e_val = L.build_load e' "actual_pitch_struct" builder in 
 *)      	          ignore(L.build_store e' pitch_pointer builder);
      		  in
      			(* iterates through pitches with deal_with_pitch*)
      			List.iteri deal_with_pitch es;
            let c_pointer_arr = L.build_in_bounds_gep c_struct [| L.const_int i32_t 0; L.const_int i32_t 1 |] "struct_c_pointer" builder in
            ignore(L.build_store arr_chord_malloc c_pointer_arr builder); c_struct	  
	     |  _ ->
	      let s_list = L.build_alloca (snd (stype_of_typ t)) "array_struct" builder in
      		let pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
      		  ignore(L.build_store (L.const_int i32_t (List.length es)) pointer builder);
      		let arr_alloc = L.build_array_alloca (fst (stype_of_typ t)) (L.const_int i32_t (List.length es)) "array" builder
      		in 
      		  let deal_with_element index e =  
      		    let pointer = L.build_gep arr_alloc [| (L.const_int i32_t index)|] "elem" builder in 
      		    let e' = expr builder e in 
      		      ignore(L.build_store e' pointer builder)
      		  in
      		   List.iteri deal_with_element es;
      		let pointer_arr = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 1 |] "actual_list" builder in 
      		  ignore(L.build_store arr_alloc pointer_arr builder);  s_list
   )


   | A.ASubset(e1, e2, _)  ->
      let s_list = expr builder e1 in 
      let index  = expr builder e2 in 
        let pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 0 |] "length" builder in
          let length = L.build_load pointer "size" builder in 
            let list_pointer = L.build_in_bounds_gep s_list [| L.const_int i32_t 0; L.const_int i32_t 1 |] "cur_list_ptr" builder in 
            let act_list = L.build_load list_pointer "cur_list" builder in
            let pointer_to_element = L.build_gep act_list [| index |] "pointer_to_element" builder in 
            L.build_load pointer_to_element "tmp" builder


(*           let var = try Hashtbl.find main_vars s
                    with Not_found -> raise (Failure (s ^ " Not Found!"))
            in 
              let head = L.build_load var "head" builder in 
              let pointer = L.build_gep head [| (L.const_int i32_t index) |] "pointer" builder in 
               L.build_load pointer "tmp" builder
 *)  
	    
    | A.ABlock(es, t) -> 
        (match es with 
        e::e1::rest -> ignore(expr builder e); expr builder (A.ABlock(e1::rest,
        t))
      | [e] -> expr builder e)
    | A.APreop(op, e, _) ->
       let e' = expr builder e in
       (match op with
		     A.Neg     -> L.build_neg
		   | A.Not     -> L.build_not
       ) e' "tmp" builder
(*
    | A.AMuPreop(op, e, _) ->
	(* given pointer to pitch array, memory operations for adding or subtracting to position 0 of pitch element *)
	(* index is needed so map will accept it *)
	let interior_operation index pitch builder1 =
	let prefield_pointer=L.build_gep pitch [| (L.const_int i32_t 0)|] "prefield_elem" builder1 in
	let cur_prefield = L.build_load prefield_pointer "cur_prefield" builder1 in
	(match op with
	  A.AOup -> 
		let new_prefield = L.build_add cur_prefield (L.const_int i32_t 1) builder1

	  A.AOdown ->
		let new_prefield = L.build_sub cur_prefield (L.const_int i32_t 1) builder1

	)  in
	ignore(L.build_store new_prefield prefield_pointer builder); in
	(* match different things mupreops could be applied to, there are 3 *)
         (match e with
	  |  A.APitch -> 
		let e'=expr builder e in
		interior_operation e' builder
	  
	  | A.AChord ->
		let e' = expr builder e in
		map (get_list e' builder) interior_operation
		

	  | A.AChordlist ->
		let e' = expr builder e in 

	)

*)
    | A.AAssign (s, e, t) -> let e' = expr builder e in 
          let var = try Hashtbl.find main_vars s 
                    with Not_found ->  
                    let local_var = L.build_alloca (ltype_of_typ t) s builder in 
                        Hashtbl.add main_vars s local_var; local_var in
                ignore (L.build_store e' var builder); e' 
(*    | A.ACall (A.AID("Map", _), act, _) -> 
	let func = expr builder (List.hd act) in
	let lst = expr builder (List.hd (List.tl act)) in
	let wrapper f=f in 
	map lst (wrapper func); L.const_int i32_t 1
*)	

    | A.ACall (A.AID("Printint", _), [e], _) ->
       L.build_call printf_func [| int_format_str ; (expr builder e) |]  "printf" builder
    | A.ACall (A.AID("Printstr", _), [e], _) ->
       L.build_call printf_func [| str_format; (expr builder e) |] "printf" builder
    | A.ACall (A.AID("Printfloat", _), [e], _) ->
       L.build_call printf_func [| float_format; (expr builder e) |] "printf" builder
    | A.ACall (A.AID("Printlist", _), [e], _) -> 
      let printfun value index builder = L.build_call printf_func [|int_no_line ; value |] "printf" builder in
  		let s_list= expr builder e in
  		map s_list printfun; L.const_int i32_t 1	
    | A.ACall(A.AID("Printrlist", _), [e], _) -> 
      let printfun value index builder = L.build_call printf_func [|float_no_line ; value |] "printf" builder in
      let s_list= expr builder e in
      map s_list printfun; L.const_int i32_t 1




	(* assumed order of acutals: pitchlist, rhythmlist, modelist, start note *)
    | A.ACall (A.AID("Synth", _), act, _) -> 
	(*extract the actuals *)
	let clist = expr builder (List.hd act) in
	let clist_len = get_length (clist, builder) in
	let rlist = expr builder (List.hd (List.tl act)) in
	let act_rlist = get_list(rlist, builder) in 
	let modelist = expr builder (List.hd (List.tl (List.tl act))) in
	let act_modelist = get_list(modelist, builder) in 
	let mode_len = get_length(modelist, builder) in 
	let start_pitch = expr builder (List.hd (List.tl (List.tl (List.tl act)))) in
	(*build the nessesary structures to pass into c function - plist as non-struct int***, list of chord lengths, return-arr *)
	
	(*malloced structure that contains lengths of chords *)
	let chord_lengths = L.build_array_malloc i32_t clist_len "return_arr" builder in 
	(*malloced structure that normalized pitch array (no octaves or accidnetals) will be built into. This is passed into C synth function *)
  	let clear_cl_list = L.build_array_malloc i32p_t clist_len "return_arr" builder in
	(*building non-struct chord : Note that this refers to both the normal builder and the builder inside the while loop (builder1)*)
	let passed_cl_list =L.build_array_malloc i32pp_t clist_len "norm_arr" builder in 
		let chord_func value1 index builder1= 
		(* for chord_lengths *)
			let pointer_to_ret_elem = L.build_in_bounds_gep passed_cl_list [| index |] "cur_val" builder1 in 
			let new_elem_list = L.build_extractvalue value1 1 "stuff" builder1 in
      let chord_len_pointer =  L.build_in_bounds_gep chord_lengths [| index |] "len" builder1 in
      let new_elem_len = L.build_extractvalue value1 0 "oldlen" builder1 in 
      let clear_list_pointer = L.build_in_bounds_gep clear_cl_list [| index |] "len" builder1 in
      let new_clear_arr = L.build_array_malloc i32_t new_elem_len "clear_cl_list_elem" builder1 in 
      ignore(L.build_store new_clear_arr clear_list_pointer builder1);
      ignore(L.build_store new_elem_len chord_len_pointer builder1);
      ignore(L.build_store new_elem_list pointer_to_ret_elem builder1);
		in

			
	   map clist (* (get_list(clist, builder)) *) chord_func;

        L.build_call synth_func [| (* int *** *)passed_cl_list; (* int  *)clist_len;     
        (* int * *)chord_lengths; (* int  *) start_pitch; (* int *  *)act_modelist; 
        (* int  *)mode_len; (* double *  *)act_rlist; (* int ** *)clear_cl_list  |] "synth" builder	
	
	(* int ***chordlist, int len_chordlist, int *chord_lengths, 
  int start_pitch, int * modelist, int mode_length, double *rhythmlist, 
  int **pure_chord_arr *)
                      	
			
		
	
    | A.ACall (A.AID(s, _), act, _) ->
       let (fdef, fdecl) = Hashtbl.find function_defs s  in
       let actuals = List.rev (List.map (expr builder) (List.rev act)) in
(* let result = (match fdecl.A.typ with A.Void -> ""
    | _ -> f ^ "_result") in *)
       L.build_call fdef (Array.of_list actuals) "" builder

    | A.AIf(e1, e2, e3, _) -> 
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
    | A.AFun(fid, arg_list, e, typ) -> 
      (* and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) *)
      let ftype = L.function_type i32_t [||] in
      let the_function = L.define_function fid ftype the_module in
      Hashtbl.add function_defs fid (the_function, A.AFun(fid, arg_list, e, typ)); 
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
