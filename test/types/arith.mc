include "string.mc"

lang Ast
  syn Tm

-- Do we need to define?
--  sem show : Ast.tm + a -> String
end



lang Arith = Ast
--  data Option =  -- This is a closed data type.

  syn Tm =  -- +=
  | TmLit {val: Int}
  | TmAdd {left: Tm, right: Tm}

  sem idArith = --| idArith : Arith.Tm -> Ast.Tm
  | TmLit r -> TmLit r
  | TmAdd r -> TmAdd r
  --| TmAdd {left:TmLit{_}} -> TmAdd
  --| TmAdd {left:TmAdd{_}} -> TmAdd

  sem show =  --|  show : Arith.Tm + a -> String                -- This extensible.
  | TmLit r -> int2string r.val
  --| TmAdd r -> concat (concat (show (r.left))  " + ") (show (r.right))
  | TmAdd r -> concat (concat (show (idArith r.left))  " + ") (show (r.right))

end

lang Bool = Ast
  syn Tm = -- +=
  | TmTrue {}
  | TmFalse {}
  | TmIf {cnd: Tm, thn: Tm, els: Tm }

  sem show = --| show : Bool.Tm + a -> String
  | TmTrue _ -> "true"
  | TmFalse _ -> "false"
  | TmIf r -> concat (concat (concat (concat (concat (concat
                "(if " (show r.cnd)) " then ") (show r.thn)) " else ") (show r.els)) ")"
end

lang ArithBool = Arith + Bool

mexpr

use ArithBool in
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
