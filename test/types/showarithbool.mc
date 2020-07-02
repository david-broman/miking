include "string.mc"

lang TermSyntax
  syn Tm
end


lang ArithSyntax = TermSyntax
  syn Tm +=
  | TmLit {val: Int}
  | TmAdd {left: Tm, right: Tm}
end

lang BoolSyntax = TermSyntax
  syn Tm +=
  | TmTrue {}
  | TmFalse {}
  | TmIf {cnd: Tm, thn: Tm, els: Tm }
end

lang ShowArith = ArithSyntax
  sem id : Tm -> Tm =
  | TmLit r -> TmLit r
  | TmAdd r -> TmAdd r

  sem show : Tm -> String =
  | TmLit r -> int2string r.val
  | TmAdd r -> concat (concat (show (id r.left))  " + ") (show (r.right))
  -- If we write ShowArith.id, we should get a type error because id is then closed.
end

lang ShowBool = BoolSyntax
  -- If we remove id in ShowBool, we should get a type error
  sem id : Tm -> Tm =
  | TmTrue r -> TmTrue r
  | TmFalse r -> TmFalse r
  | TmIf r -> TmIf r

  sem show : Tm -> String =
  | TmTrue _ -> "true"
  | TmFalse _ -> "false"
  | TmIf r -> concat (concat (concat (concat (concat (concat
                "(if " (show r.cnd)) " then ") (show r.thn)) " else ") (show r.els)) ")"
end

lang ShowArithBool = ShowArith + ShowBool

mexpr

use ShowArithBool in
let tmLit = lam x. TmLit{val=x} in
let tmAdd = lam x. lam y. TmAdd{left=x,right=y} in
let tmTrue = TmTrue {} in
let tmFalse = TmFalse {} in
let tmIf = lam cnd. lam thn. lam els. TmIf {cnd=cnd, thn=thn, els=els} in

utest show (tmAdd (tmLit 1) (tmLit 2)) with "1 + 2" in
utest show tmTrue with "true" in
utest show (tmIf tmTrue (tmLit 1) (tmLit 2)) with "(if true then 1 else 2)" in

utest show (tmAdd (tmIf tmTrue (tmLit 1) (tmLit 2)) (tmLit 3)) with "(if true then 1 else 2) + 3" in


()
