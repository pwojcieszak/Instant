module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  , Bool(..)
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO          ( writeFile )
import System.Process     ( callCommand )
import System.Directory   ( createDirectoryIfMissing ) 
import System.FilePath    ( takeBaseName, takeDirectory )
import Control.Monad      ( when )

import AbsInstant   ( Program(..) )
import LexInstant   ( Token, mkPosToken )
import ParInstant   ( pProgram, myLexer )
import PrintInstant ( Print, printTree )
import SkelInstant  ()
import GeneratorLLVM ( generateLLVM )

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p f

run :: Verbosity -> ParseFun Program -> FilePath -> String -> IO ()
run v p f s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      let llvmCode = generateLLVM tree 
      let baseName = takeBaseName f
      let outputDir = takeDirectory f
      createDirectoryIfMissing True outputDir

      let llFilePath = outputDir ++ "/" ++ baseName ++ ".ll"
      let bcFilePath = outputDir ++ "/" ++ baseName ++ ".bc"

      writeFile llFilePath llvmCode
      callCommand $ "llvm-as " ++ llFilePath ++ " -o " ++ bcFilePath
      -- TODO usunac
      callCommand $ "lli " ++ bcFilePath 

  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> getContents >>= run 2 pProgram ""
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

