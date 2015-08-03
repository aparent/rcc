module SimplifyJanus
  ( simplify
  ) where

import Prelude
import ParseJanus

simplify :: Janus -> Janus
simplify (Janus (decl,stmt)) = Janus (decl, simplifyStmt stmt)

simplifyStmt :: Stmt -> Stmt
simplifyStmt s =
  case s of
    ModStmt var op aExpr -> ModStmt var op (simplifyAExpr aExpr)
    IfElse cond a b asrt -> IfElse cond (simplifyStmt a) (simplifyStmt b) asrt
    Seq stmts -> Seq $ map simplifyStmt stmts
    _ -> s

simplifyAExpr :: AExpr -> AExpr
simplifyAExpr expr =
  simp $
  case expr of
    ABinary op a b -> ABinary op (simplifyAExpr a) (simplifyAExpr b)
    _ -> expr
  where simp e =
          case e of
            ABinary Add (Const 0) x          -> x
            ABinary Add x (Const 0)          -> x
            ABinary Add (Const a) (Const b)  -> Const $ a + b
            ABinary Sub x (Const 0)          -> x
            ABinary Mult _ (Const 0)         -> Const 0
            ABinary Mult (Const 0) _         -> Const 0
            ABinary Mult x (Const 1)         -> x
            ABinary Mult (Const 1) x         -> x
            ABinary Mult (Const a) (Const b) -> Const $ a * b
            _                                -> e

