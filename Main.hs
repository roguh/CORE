module Main where

import Data.Text (pack, unpack)
import qualified Data.Text.IO as TIO

import Control.Monad

import Core.Types
import Core.Parser
import Core.PrettyPrint
import Core.Util.Heap
import Core.Compiler.TemplateMk1
import Core.Compiler.TemplateMk3
import Core.Compiler.GMachineMk1
import Core.Compiler.GMachineMk2
import Core.Compiler.GMachineMk3
import Core.Compiler.GMachineMk4
import Core.Compiler.GMachineMk5
import Core.Compiler.GMachineMk6
import Core.Compiler.GMachineMk7

import Core.Compiler

import System.Environment

main = do
    as <- getArgs
    if null as then loop else mapM_ (\f -> putStr ("\n\nFile: " ++ f ++ "\n") >> parsePrintFile f) as
  where loop = do
          putStrLn "type a filename"
          fname <- getLine
          parsePrintFile fname
          loop
