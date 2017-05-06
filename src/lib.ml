open Ast

module StringMap = Map.Make(String)

let stdlib = [
    ("Printint", TFun([TInt], TString));
    ("Printstr", TFun([TString], TString));
    ("Printfloat", TFun([TFloat], TString));
];;

let predefined = 
  List.fold_left (fun env (id, t) -> StringMap.add id t env) 
  StringMap.empty stdlib
;;


