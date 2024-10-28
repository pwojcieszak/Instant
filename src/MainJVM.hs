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
import GeneratorJVM ( generateJVM )

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
      let baseName = takeBaseName f
      let jvmCode = generateJVM tree baseName
      let outputDir = takeDirectory f
      createDirectoryIfMissing True outputDir

      let jFilePath = outputDir ++ "/" ++ baseName ++ ".j"

      writeFile jFilePath jvmCode
      callCommand $ "java -jar lib/jasmin.jar -d " ++ outputDir ++ " " ++ jFilePath ++ " > /dev/null"

  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  (files)         Parse and compile content of files verbosely."
    , "  -s (files)      Silent mode. Parse and compile content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> usage
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

