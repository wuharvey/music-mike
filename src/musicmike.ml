(* Top-level of the MusicMike compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | LLVM_IR | Sast | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-s", Sast);(* Print the AST only *)
                  ("-a", Ast);
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.next_token lexbuf in
(*  Semant.check ast; *) 
    Semant.check ast;
  match action with
  | Ast -> print_string (Ast.string_of_program ast)
  | Sast -> let sast = Infer.typecheck ast in print_string (Ast.string_of_inferred sast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
