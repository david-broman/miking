
include "seq.mc"
include "string.mc"
include "mexpr/info.mc"

lang Wacc

  syn Token =
  | TokKeyword Int
  | TokIdent String
  | TokNum

  syn NonTerminal =
  | NTerm (Info, String)
  | TKeyword (Info, String)
  | TIdent (Info)
  | TNum (Info)

  type Action = (Info, String)

  syn Prod =
  | Prod(Info, [NonTerminal], Action)

  syn Rule =
  | Rule (Info, String, [Prod])

  type Grammar = [Rule]

  sem prod nonterms =
    | action -> Prod(NoInfo (), nonterms, (NoInfo (), action))

  sem rule name =
    | prod -> Rule(NoInfo (), name, prod)


  sem nt = | s -> NTerm (NoInfo (), s)
  sem kw = | s -> TKeyword (NoInfo (), s)
  sem ident = | _ -> TIdent (NoInfo ())
  sem num = | _ -> TNum (NoInfo ())

/--
  -- Generic function for pretty printing a grammar
  sem pprintGrammar --(ppRuleStart : Bool -> String -> String)
                    --(ppProducts : [Prod] -> String)
                    --(ppRuleEnd : String -> String) =
                    =
  | g ->
    foldl (lam a:(Bool, String). lam rule:Rule. (false, "")
    --  match (a, rule) with ((first, str), Rule(_, x, prods)) in
    --  (false, join [str, ppRuleStart first x, ppProducts prods, ppRuleEnd x])
      ) (true, "") g

  -- Generic function for pretty printing product rules
  sem pprintProducts (ppProd : [NonTerminal] -> String -> Int) =
  | prods ->
    foldl (lam a. lam prod.
      match prod with Prod(_, terms, (_, action)) in
        concat a (ppProd terms action)
    ) "" prods

  -- Defines the target language
  syn Target =
  | TargetGrammar()

  -- Takes a grammar as input and generates the target output
  sem compile grammar =
  | TargetGrammar() ->
      let ppProd = lam nterms. lam action.
        let strTerms =
          foldl (lam a. lam t.
             let str =
               match t with NTerm(_, s) then s else
               match t with TKeyword(_, s) then join ["'", s, "'"] else
               match t with TIdent(_) then "Ident" else
               match t with TNum(_) then "Num" else "" in
             join [a, " ", str]) "" nterms in
        join ["  | ", strTerms, "\n     { ", action, " }\n"]
      in
      pprintGrammar (lam. lam name. concat (name ":\n"))
                    (pprintProducts ppProd)
                    (lam. "\n")
--/


  /--
  sem grammar2string =
  | g -> foldl (lam a. lam rule.
           match rule with Rule(_, x, prods) in
           let rules =
             foldl (lam a. lam prod.
               match prod with Prod(_, tlist, (_, action)) in
               let v = foldl (lam a. lam t.
                 let str =
                   match t with NTerm(_, s) then s else
                   match t with TKeyword(_, s) then join ["'", s, "'"] else
                   match t with TIdent(_) then "Ident" else
                   match t with TNum(_) then "Num" else "" in
                 join [a, " ", str]
               ) "" tlist in
               let act = if eqi (length action) 0 then "" else
                 join ["\n     { ", action, " }"]
               in
               join [a, "  |", v, act, "\n"]
             ) "" prods in
           join [a, x, ":\n", rules, "\n"]) "" g
  --/

  -- Takes a grammar as input and returns a sequence of all keywords,
  -- where the keywords only appear once. Not the most efficient implementation.
  /--sem findKeywords =
  g ->
    let str = foldl (lam a. lam prod.
      match prod with (

    in
    distinct eqString
  --/



end

let g1 =
  use Wacc in [
      rule "expr" [
        (prod [nt "expr", kw "+", nt "term"] "ExprAdd($1, $3)"),
        (prod [nt "term"]  "")]
      rule "term" [
        (prod [nt "term", kw "*", nt "factor"] ""),
        (prod [nt "factor"] "")]
      rule "factor" [
        (prod [num ()] ""),
        (prod [kw "(", nt "expr", kw ")"] "")]
     ]


let main =
  use Wacc in
  print (compile (TargetGrammar()) g1);
  print "\nHello\n"
