
include "seq.mc"

lang Wacc

  syn NonTerminal =
  | NTerm String
  | TKeyword String
  | TIdent
  | TString
  | TNum

  type Rule = (String, [([NonTerminal], String)])

  type Grammar = [Rule]

  sem grammar2string =
  | g -> foldl (lam a. lam prod.
           match prod with (x, lst) in
           let rules =
             foldl (lam a. lam rhs.
               match rhs with (tlist, action) in
               let v = foldl (lam a. lam t.
                 let str =
                   match t with NTerm s then s else
                   match t with TKeyword s then join ["'", s, "'"] else
                   match t with TIdent () then "Ident" else
                   match t with TString () then "String" else
                   match t with TNum () then "Num" else "" in
                 join [a, " ", str]
               ) "" tlist in
               let act = if eqi (length action) 0 then "" else
                 join ["\n     { ", action, " }"]
               in
               join [a, "  |", v, act, "\n"]
             ) "" lst in
           join [a, x, ":\n", rules, "\n"]) "" g

end

let g1 =
  use Wacc in [
     ("expr",  [([NTerm "expr", TKeyword "+", NTerm "term"], "ExprAdd($1, $3)"),
                ([NTerm "term"], "")]),
     ("term",  [([NTerm "term", TKeyword "*", NTerm "factor"], ""),
                ([NTerm "factor"], "")]),
     ("factor",[([TNum ()], ""),
                ([TKeyword "(", NTerm "expr", TKeyword ")"], "")])
     ]


let main =
  use Wacc in
  print (grammar2string g1);
  print "\nHello\n"
