{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
module Core.Compiler where

import Data.Text
import Data.Text.IO
import Prelude hiding (readFile, appendFile, writeFile, putStrLn)
import Control.Monad

import Core.Types
import Core.Parser
import Core.PrettyPrint

class CoreCompiler state where
    compile :: [Supercombo Text] -> ThrowsError state
    eval    :: state -> ThrowsError [state]
    showStateTrace  :: [state] -> Text
    showIncrementalResults :: [state] -> [Text]
    showResult      :: [state] -> Text
    showResult = showStateTrace
    showIncrementalResults = (:[]) . showResult

type RunCore state = FilePath -> state -> Text -> ThrowsError [Text]

run :: CoreCompiler st => ([st] -> [Text]) -> RunCore st
run showFunc fname coreCompiler = parse fname >=> compile >=> eval >=>
    (\results -> return $ showFunc $ results `asTypeOf` [coreCompiler])

runTest = run ((:[]) . showStateTrace)
runIncremental = run showIncrementalResults
runPlain = run ((:[]) . showResult)

parseEval :: CoreCompiler state => FilePath -> state -> RunCore state -> ([Text] -> IO ()) -> Text -> IO ()
parseEval fname compiler runFunc printer txt =
    let rs = runFunc fname compiler txt
    in either (printer . (:[]) . pack) printer rs

parseEvalText = parseEval ""
parseEvalFile comp run print fname = readFile fname >>= parseEval fname comp run print

parseCompile coreCompiler str =
    (parse "" >=> compile $ pack str) `asTypeOf` Right coreCompiler

parsePrintFile fname = readFile fname >>= parsePrint fname
parsePrintText = parsePrint ""
parsePrint fname = putAndWriteTo "result" . either (pack . showErr) pprint . parse fname

putAndWriteTo fname str = putStrLn str >> writeFile fname str

runCore compiler = parseEvalText compiler runPlain (print . Prelude.last) . pack
