module Main where

import qualified Data.Text.IO as TIO

import Control.Monad

import System.IO
import System.CPUTime
import System.IO.Error
import System.Environment
import System.Console.Haskeline

import Core.Types
import Core.Parser
import Core.PrettyPrint

--import Core.Compiler.TemplateMk1
import Core.Compiler.TemplateMk3
--import Core.Compiler.GMachineMk1
--import Core.Compiler.GMachineMk2
--import Core.Compiler.GMachineMk3
--import Core.Compiler.GMachineMk4
--import Core.Compiler.GMachineMk5
import Core.Compiler.GMachineMk6
--import Core.Compiler.GMachineMk7

import Core.Compiler
import Core.Util.Prelude

compilers =
    [ ("GMachine Mk1", gmachinemk1), ("GMachine Mk2", gmachinemk2)
    , ("GMachine Mk3", gmachinemk3), ("GMachine Mk4", gmachinemk4)
    , ("GMachine Mk5", gmachinemk5), ("GMachine Mk6", gmachinemk6)
    , ("GMachine Mk7", gmachinemk7)
    , ("Template instantiation Mk1", templateMk1)
    , ("Template instantiation Mk3", templateMk3) ]

data Config = Config { _compiler ::  () , _run :: (), _timeIt :: Bool }
defaultConfig = Config

main :: IO ()
main =
    do
        hSetBuffering stdin NoBuffering
        as <- getArgs
        case length as of
            0 -> do putStrLn (helpStr ++ "\n\n" ++ creditsStr)
                    runInputT haskelineSettings (loop defaultConfig)
            _ -> mapM_ (evalFile $ readEvalLines True) as
     where
        loop conf = do
             userSays <- getInputLine "core >>> "
             maybe
                (return ())
                (\expr ->
                    maybe
                    (eval expr >> loop) -- eval here
                    (lookup (head $ words expr) replFuncs)
                ) userSays
        time action = do
            a <- liftIO getCPUTime
            res <- action
            b <- liftIO getCPUTime
            outputStrLn $ "time: " ++ show (fromIntegral (b - a) / 1e12)
            return res
        replFuncs =
            [ (".?",    \conf -> outputStrLn helpStr >> loop)
            , (".help", \conf -> TIO.print (T.unlines primitives) >> loop e)
            , (".toggletime", \conf -> do
                outputStrLn $ "timing enabled: " ++ show (not (_timeIt conf))
                loop $ conf{ _timeIt = not $ _timeIt conf })
            , (".quit", \conf -> outputStrLn "au revoir")
            ]
        helpStr = "Type a core expression or a REPL command: " ++ unwords (map fst replFuncs)
        creditsStr = unlines
            [ "Written in VIM and compiled by Glasgow's Haskell Compiler" ]

        evalFile evaluator fname = liftIO $ catchIOError
                (parseEvalFile comp run print fname)
                (putStrLn . (("failure reading " ++ fname ++ "\n")++) . show)

        primitives = map _name prelude
        coreCompletions word =
            return $ filter (T.isPrefixOf word)
                    primitives -- (map fst readOnlys ++ map fst bindings ++ map fst replFuncs)
                    --- what to autocomplete...
                >>= \rep -> return Completion { replacement = rep, display = rep
                                              , isFinished = rep == word }
        haskelineSettings = flip setComplete defaultSettings $ \line -> do
            (_, cs1) <- completeWord Nothing " \t\n\r\f\v" coreCompletions line
            (s, cs2) <- completeFilename line
            return (s, cs1 ++ cs2)


