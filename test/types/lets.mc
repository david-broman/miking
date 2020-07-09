
-- To test the type pretty printing, run the following:
--   mi test/types/mexpr.mc --debug-symbolize --no-prelude

mexpr


-- Show constructors with and without types
type Tree in
con Foo1 : Int -> Tree in
con Foo2 in

-- lets and lambdas with and without types
let f1 = lam x:Int. lam y:Int. addi x y in
let f2 : Int -> Int -> Int  = lam x. lam y. addi x y in

-- recursive lets with and without types
recursive
let g1 : Bool -> Bool = lam x. x
let g2 = lam y:Int. y
in


()
