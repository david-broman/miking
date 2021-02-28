let enable_typecheck = ref false

let typecheck tm =
  if not !enable_typecheck then tm
  else (
    Printf.printf "TYPECHECKING!\n" ;
    tm )
