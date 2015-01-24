{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
module Core.Compiler where

import Data.Text
import Data.Text.IO as TIO
import Prelude hiding (readFile, appendFile, writeFile, putStrLn)
import Control.Monad

import Core.Types
import Core.Parser
import Core.PrettyPrint

import Core.Util.Heap

data State = State { output :: Text, statistics :: Text }

-- The last state in the result of compiler is expected to hold the final
-- output
data Compiler = Compiler { compilerName :: Text
                         , compiler     :: CoreProgram -> ThrowsError [State] }

type RunCore = FilePath -> Compiler -> Text -> ThrowsError [Text]

run, runTest :: RunCore
run fname (Compiler{ compiler=comp }) =
    parse fname >=> comp >=> return . Prelude.map output
runTest fname (Compiler{ compiler=comp }) =
    parse fname >=> comp >=> return . Prelude.map
    (\(State a b) -> b `append` "\n\nFinal result: " `append` a)

parseEval :: FilePath -> Compiler -> RunCore -> ([Text] -> IO ()) -> Text -> IO ()
parseEval fname compiler runFunc printer =
    let rs = runFunc fname compiler
    in either (printer . (:[]) . pack) printer . rs

parseEvalText = parseEval ""
parseEvalFile comp run print fname = readFile fname >>= parseEval fname comp run print

parsePrintFile fname = readFile fname >>= parsePrint fname
parsePrintText = parsePrint ""
parsePrint fname = print . either (pack . showErr) pprint . parse fname

runCore compiler = parseEvalText compiler run (putStrLn . Prelude.last) . pack
