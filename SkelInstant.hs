-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelInstant where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsInstant

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsInstant.Ident -> Result
transIdent x = case x of
  AbsInstant.Ident string -> failure x

transProgram :: AbsInstant.Program -> Result
transProgram x = case x of
  AbsInstant.Prog stmts -> failure x

transStmt :: AbsInstant.Stmt -> Result
transStmt x = case x of
  AbsInstant.SAss ident exp -> failure x
  AbsInstant.SExp exp -> failure x

transExp :: AbsInstant.Exp -> Result
transExp x = case x of
  AbsInstant.ExpAdd exp1 exp2 -> failure x
  AbsInstant.ExpSub exp1 exp2 -> failure x
  AbsInstant.ExpMul exp1 exp2 -> failure x
  AbsInstant.ExpDiv exp1 exp2 -> failure x
  AbsInstant.ExpLit integer -> failure x
  AbsInstant.ExpVar ident -> failure x
