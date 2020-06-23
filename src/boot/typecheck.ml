(*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

   File typechecker.ml includes the prototype implementation of the typechecker
   for MLang and MExpr.
*)

open Ast

exception Typecheck_error of Msg.message

(*
 let parse_error
	raise (Lex_error (PARSE_ERROR,ERROR,mkinfo_utf8_fast c,[s])) }
*)


(* Perform the type checking of the whole program. Raises an exception if a type error is found.
   Otherwise, it returns the same program as given as the input. *)
let check = function
  | Program(l,i,t) -> Program(l,i,t)
