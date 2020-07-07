(*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

   File typechecker.ml includes the prototype implementation of the typechecker
   for MLang and MExpr.
*)

open Ast
open Ustring.Op
open Msg


let rec type_eq ty1 ty2 =
  match ty1,ty2 with
  | TyUnit,TyUnit | TyDyn,TyDyn | TyBool,TyBool | TyInt,TyInt |
    TyFloat,TyFloat | TyChar,TyChar -> true
  | TyArrow(ty1a,ty2a),TyArrow(ty1b,ty2b) -> type_eq ty1a ty1b && type_eq ty2a ty2b
  | TySeq(tya),TySeq(tyb) -> type_eq tya tyb
  (* TODO: Add missing types *)
  | _ -> false

let type_of_const fi = function
  | CBool(_) -> TyBool
  | CInt(_) -> TyInt
  | CFloat(_) -> TyFloat
  | CChar(_) -> TyChar
  | _ -> raise_error fi "Type of constant not implemented"


(* Type checking a term given a typing environment. *)
let rec infer env = function
  | TmConst(fi,c) -> type_of_const fi c
  | TmSeq(fi,tms) ->
    (match Mseq.front tms with
               (* TODO: implement empty sequences using type variables *)
     | None -> raise_error fi "Type error. Empty sequences not implemented."
     | Some(tl,hd) ->
       let ty = infer env hd in
       Mseq.iter (fun t -> if type_eq ty (infer env t) then () else
           raise_error fi "Inconsistent types types in sequence") tl;
       TySeq(ty))
  | _ -> let _ = env in TyDyn


let check env = function
  | _ -> let _ = env in raise_error NoInfo "Not implemented"


(* Perform type checking. If there is an error, an exception is raised, else the original
   term is returned *)
let typecheck tm =
  if !enable_type_check then
    uprint_endline (us"Type checking OK. Type: " ^. (infer [] tm |> Pprint.ustring_of_ty));
  tm
