module GeneratorJVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [(String, Int)]

updateMaxStackDepth :: Int -> Int -> Int -> Int
updateMaxStackDepth d1 d2 maxDepth =
  let new = max d1 d2
  in if new > maxDepth
     then new
     else maxDepth

generateJVM :: AbsInstant.Program -> String -> String
generateJVM (Prog stmts) baseName =
  let (code, _, varMap, _, maxStackDepth) = foldl generateStmt ([], 0, [], 0, 0) stmts
  in ".class public " ++ baseName ++ "\n" ++
     ".super java/lang/Object\n\n" ++
     ".method public static main([Ljava/lang/String;)V\n" ++
     ".limit stack " ++ show(100) ++ "\n" ++            -- TODO wrócić po optymalizacji (maxStackDepth)
     ".limit locals " ++ show((length varMap) + 1) ++ "\n" ++
     concat (reverse code) ++
     "  return\n" ++
     ".end method\n"


generateStmt :: ([String], Int, VarMap, Int, Int) -> AbsInstant.Stmt -> ([String], Int, VarMap, Int, Int)
generateStmt (code, localVarIndex, varMap, stackDepth, maxStackDepth) (SAss (Ident ident) exp) =
  let (expCode, newReg, newStackDepth, newMaxStackDepth) = generateExp code localVarIndex varMap (stackDepth + 1) maxStackDepth exp
      varIndex = case lookup ident varMap of
                   Just index -> index
                   Nothing    -> localVarIndex
      storeCode = if varIndex <= 3
                  then "  istore_" ++ show varIndex ++ "\n"
                  else "  istore " ++ show varIndex ++ "\n"
      (newVarMap, newLocalVarIndex) = if lookup ident varMap == Nothing
                  then ((ident, varIndex) : varMap, newReg + 1)
                  else (varMap, newReg)
  in (storeCode : expCode, newLocalVarIndex, newVarMap, newStackDepth - 1, newMaxStackDepth)

generateStmt (code, reg, varMap, stackDepth, maxStackDepth) (SExp exp) =
  let printHead = "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
      (expCode, newReg, newStackDepth, newMaxStackDepth) = generateExp (printHead : code) reg varMap (stackDepth + 2) maxStackDepth exp
      printTail = "  invokevirtual java/io/PrintStream/println(I)V\n"
  in (printTail : expCode, newReg, varMap, newStackDepth - 2, newMaxStackDepth)


generateExp :: [String] -> Int -> VarMap -> Int -> Int -> AbsInstant.Exp -> ([String], Int, Int, Int)
generateExp code reg varMap stackDepth maxStackDepth (ExpAdd e1 e2) = generateBinaryOp code reg varMap stackDepth maxStackDepth e1 e2 "iadd"
generateExp code reg varMap stackDepth maxStackDepth (ExpSub e1 e2) = generateBinaryOp code reg varMap stackDepth maxStackDepth e1 e2 "isub"
generateExp code reg varMap stackDepth maxStackDepth (ExpMul e1 e2) = generateBinaryOp code reg varMap stackDepth maxStackDepth e1 e2 "imul"
generateExp code reg varMap stackDepth maxStackDepth (ExpDiv e1 e2) = generateBinaryOp code reg varMap stackDepth maxStackDepth e1 e2 "idiv"
generateExp code reg _ stackDepth maxStackDepth (ExpLit n) =
  let newMaxStackDepth = max stackDepth maxStackDepth
      litCode = case () of
        _ | n >= -1 && n <= 5    -> "  iconst_" ++ show n ++ "\n"
          | n >= -128 && n <= 127 -> "  bipush " ++ show n ++ "\n"
          | n >= -32768 && n <= 32767 -> "  sipush " ++ show n ++ "\n"
          | otherwise             -> "  ldc " ++ show n ++ "\n"
  in (litCode : code, reg, stackDepth, newMaxStackDepth)

generateExp code reg varMap stackDepth maxStackDepth (ExpVar (Ident ident)) =
  let newMaxStackDepth = max stackDepth maxStackDepth
      varCode = case lookup ident varMap of
        Just index
          | index >= 0 && index <= 3 -> "  iload_" ++ show index ++ "\n"
          | otherwise -> "  iload " ++ show index ++ "\n"
        Nothing -> error $ "Variable " ++ ident ++ " not found."
  in (varCode : code, reg, stackDepth, newMaxStackDepth)

generateBinaryOp :: [String] -> Int -> VarMap -> Int -> Int -> AbsInstant.Exp -> AbsInstant.Exp -> String -> ([String], Int, Int, Int)
generateBinaryOp code reg varMap stackDepth maxStackDepth e1 e2 op =
  let (code1, reg1, stackDepth1, maxStackDepth1) = generateExp code reg varMap (stackDepth + 1) maxStackDepth e1
      (code2, reg2, stackDepth2, maxStackDepth2) = generateExp code1 reg1 varMap (stackDepth + 1) maxStackDepth e2
      newMaxStackDepth = updateMaxStackDepth stackDepth1 stackDepth2 maxStackDepth
      operationCode = "  " ++ op ++ "\n"
  in (operationCode : code2, reg2, stackDepth - 1, newMaxStackDepth)