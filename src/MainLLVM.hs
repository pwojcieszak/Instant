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
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree

      let llvmCode = generateLLVM tree 

      createDirectoryIfMissing True "foo/bar"
      let llFilePath = "foo/bar/baz.ll"
      writeFile llFilePath llvmCode

      let bcFilePath = "foo/bar/baz.bc"
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

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

