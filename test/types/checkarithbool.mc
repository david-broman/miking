
include "string.mc"
include "showarithbool.mc"


lang TypeSyntax
  syn Ty
end


lang CheckArith = ArithSyntax + TypeSyntax
  syn Ty +=
  | TyInt {}

  ext ExtTm of Tm =
  | TmLit {ty: Ty}
  | TmAdd {ty: Ty}

  sem getType: all e :: ExtTm * tm. e * ExtTm * tm -> Ty =
  | TmLit r -> r.ty
  | TmAdd r -> r.ty

  sem typecheck: all e :: ExtTm * Tm. e * tm -> e * ExtTm * tm =
  | TmLit r -> TmLit {r with ty = TyInt{}}
  | TmAdd r ->
    let left2 = typecheck r.left in
    let right2 = typecheck r.right in
    match getType left2 with TyInt{} then
      match getType right2 with TyInt{} then
        TmAdd {{{r with left = left2} with right = right2} with ty = TyInt{}}
        -- Note: we need to automatically extend the term to include a ty field
      else error "Type error"
    else error "Type error"
end

lang ShowCheck = CheckArith + ShowArith

mexpr

use ShowCheck in
let tm1 = TmAdd{left=TmLit{val=1}, right=TmLit{val=2}} in
let tm2 = typecheck tm1 in
utest getType tm2 with TyInt{} in
utest show tm2 with "1 + 2" in
()
