module GeneratorJVM where

import qualified AbsInstant
import AbsInstant (Program(..), Stmt(..), Exp(..), Ident(..))

type VarMap = [(String, Int)]

isSimpleExp :: Exp -> Bool
isSimpleExp (ExpLit _) = True
isSimpleExp (ExpVar _) = True
isSimpleExp _ = False

generateJVM :: AbsInstant.Program -> String -> String
generateJVM (Prog stmts) baseName =
  let (code, _, varMap, maxStackSize) = foldl generateStmt ([], 1, [], 0) stmts
  in ".class public " ++ baseName ++ "\n" ++
     ".super java/lang/Object\n\n" ++
     ".method public static main([Ljava/lang/String;)V\n" ++
     ".limit stack " ++ show(maxStackSize) ++ "\n" ++
     ".limit locals " ++ show((length varMap) + 1) ++ "\n" ++
     concat (reverse code) ++
     "  return\n" ++
     ".end method\n"


generateStmt :: ([String], Int, VarMap, Int) -> AbsInstant.Stmt -> ([String], Int, VarMap, Int)
generateStmt (code, localVarIndex, varMap, maxStackSize) (SAss (Ident ident) exp) =
  let (expCode, newIndex, stackSize) = generateExp code localVarIndex varMap 0 exp
      varIndex = case lookup ident varMap of
                   Just index -> index
                   Nothing    -> localVarIndex
      storeCode = if varIndex <= 3
                  then "  istore_" ++ show varIndex ++ "\n"
                  else "  istore " ++ show varIndex ++ "\n"
      (newVarMap, newLocalVarIndex) = if lookup ident varMap == Nothing
                  then ((ident, varIndex) : varMap, newIndex + 1)
                  else (varMap, newIndex)
  in ((storeCode : expCode) ++ code, newLocalVarIndex, newVarMap, max stackSize maxStackSize)

generateStmt (code, reg, varMap, maxStackSize) (SExp exp) =
  let printHead = "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
      (expCode, newReg, stackSize) = 
          if isSimpleExp exp 
          then generateExp code reg varMap 1  exp
          else generateExp code reg varMap 0 exp
      printTail = "  invokevirtual java/io/PrintStream/println(I)V\n"
      finalCode = if isSimpleExp exp 
                  then (printTail : expCode) ++ (printHead : code)
                  else (printHead ++ "  swap\n" ++ printTail) : expCode ++ code
  in (finalCode, newReg, varMap, max stackSize maxStackSize)


generateExp :: [String] -> Int -> VarMap -> Int -> AbsInstant.Exp -> ([String], Int, Int)
generateExp code reg varMap stackSize (ExpAdd e1 e2) = generateBinaryOp code reg varMap stackSize e1 e2 "iadd"
generateExp code reg varMap stackSize (ExpSub e1 e2) = generateBinaryOp code reg varMap stackSize e1 e2 "isub"
generateExp code reg varMap stackSize (ExpMul e1 e2) = generateBinaryOp code reg varMap stackSize e1 e2 "imul"
generateExp code reg varMap stackSize (ExpDiv e1 e2) = generateBinaryOp code reg varMap stackSize e1 e2 "idiv"
generateExp _ reg _ stackSize (ExpLit n) =
  let litCode =
        if n >= -1 && n <= 5 then
          "  iconst_" ++ show n ++ "\n"
        else if n >= -128 && n <= 127 then
          "  bipush " ++ show n ++ "\n"
        else if n >= -32768 && n <= 32767 then
          "  sipush " ++ show n ++ "\n"
        else
          "  ldc " ++ show n ++ "\n"
  in ([litCode], reg, stackSize + 1)

generateExp _ reg varMap stackSize (ExpVar (Ident ident)) =
  let varCode = case lookup ident varMap of
        Just index ->
          if index >= 0 && index <= 3 then
            "  iload_" ++ show index ++ "\n"
          else
            "  iload " ++ show index ++ "\n"
        Nothing -> error $ "Variable " ++ ident ++ " not found."
  in ([varCode], reg, stackSize + 1)

generateBinaryOp :: [String] -> Int -> VarMap -> Int -> AbsInstant.Exp -> AbsInstant.Exp -> String -> ([String], Int, Int)
generateBinaryOp code reg varMap stackSize e1 e2 op =
  let (code1, reg1, stackSize1) = generateExp code reg varMap stackSize e1
      (code2, reg2, stackSize2) = generateExp code reg1 varMap stackSize e2
      (optimizedCode, newStackSize, reverseOrder) =
          if stackSize1 > stackSize2
          then (code2 ++ code1, max stackSize1 (stackSize2 + 1), False)  
          else if not(isSimpleExp e1 && isSimpleExp e2)
            then (code1 ++ code2, max stackSize2 (stackSize1 + 1), op `elem` ["isub", "idiv"])
            else (code2 ++ code1, max stackSize1 (stackSize2 + 1), False)
          
      swapInstr = if reverseOrder then "  swap\n" else ""
      operationCode = swapInstr ++ "  " ++ op ++ "\n"
  in (operationCode : (optimizedCode), reg2, newStackSize)
