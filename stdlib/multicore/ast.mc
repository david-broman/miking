-- Language fragments for multicore primitives

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang AtomicAst = ConstAst
  syn Const =
  | CAtomicMake {}
  | CAtomicGet {}
  | CAtomicExchange {}
  | CAtomicFetchAndAdd {}
  | CAtomicCAS {}
end

lang ThreadAst = ConstAst
  syn Const =
  | CThreadSpawn {}
  | CThreadJoin {}
  | CThreadGetID {}
  | CThreadID2Int {}
  | CThreadSelf {}
  | CThreadWait {}
  | CThreadNotify {}
  | CThreadCriticalSection {}
  | CThreadCPURelax {}
end

lang MExprParAst = MExprAst + AtomicAst + ThreadAst

-- Builders for atomic references
let atomicMake_ = use MExprParAst in
  lam v. appf1_ (uconst_ (CAtomicMake ())) v

let atomicGet_ = use MExprParAst in
  lam r. appf1_ (uconst_ (CAtomicGet ())) r

let atomicExchange_ = use MExprParAst in
  lam r. lam v. appf2_ (uconst_ (CAtomicExchange ())) r v

let atomicFetchAndAdd_ = use MExprParAst in
  lam r. lam v. appf2_ (uconst_ (CAtomicFetchAndAdd ())) r v

let atomicCAS_ = use MExprParAst in
  lam r. lam seen. lam v. appf3_ (uconst_ (CAtomicCAS ())) r seen v

-- Builders for threads
let threadSpawn_ = use MExprParAst in
  lam f. appf1_ (uconst_ (CThreadSpawn ())) f

let threadJoin_ = use MExprParAst in
  lam t. appf1_ (uconst_ (CThreadJoin ())) t

let threadGetID_ = use MExprParAst in
  lam t. appf1_ (uconst_ (CThreadGetID ())) t

let threadID2Int_ = use MExprParAst in
  lam id. appf1_ (uconst_ (CThreadID2Int ())) id

let threadSelf_ = use MExprParAst in
  lam u. appf1_ (uconst_ (CThreadJoin ())) u

let threadWait_ = use MExprParAst in
  lam u. appf1_ (uconst_ (CThreadWait ())) u

let threadNotify_ = use MExprParAst in
  lam id. appf1_ (uconst_ (CThreadNotify ())) id

let threadCriticalSection_ = use MExprParAst in
  lam f. appf1_ (uconst_ (CThreadCriticalSection ())) f

let threadCPURelax_ = use MExprParAst in
  lam u. appf1_ (uconst_ (CThreadCPURelax ())) u
