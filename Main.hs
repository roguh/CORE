module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T (isPrefixOf, unlines, pack, unpack)

import Control.Monad
import Control.Monad.IO.Class

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

compilers = [templateMk3, gmachineMk6]

data REPLConfig = Conf { _compiler :: Compiler, _run :: RunCore, _timeIt :: Bool }
defaultConfig = Conf gmachineMk6 run False

main :: IO ()
main =
    do
        hSetBuffering stdin NoBuffering
        as <- getArgs
        case length as of
            0 -> do putStrLn helpStr
                    runInputT haskelineSettings (loop defaultConfig)
            _ -> return ()
     where
        evalFile conf fname = catchIOError
                (parseEvalFile (_compiler conf) (_run conf) (TIO.putStrLn . last) fname)
                (putStrLn . (("failure reading " ++ fname ++ "\n")++) . show)


        coreCompletions word =
            return $ filter (T.isPrefixOf word)
                    primitives -- (map fst readOnlys ++ map fst bindings ++ map fst replFuncs)
                    --- what to autocomplete...
                >>= \rep -> return Completion { replacement = unpack rep, display = unpack rep
                                              , isFinished = rep == word }
        haskelineSettings = flip setComplete defaultSettings $ \line -> do
            (_, cs1) <- completeWord Nothing " \t\n\r\f\v" (coreCompletions . pack) line
            (s, cs2) <- completeFilename line
            return (s, cs1 ++ cs2)

primitives = map _name prelude

loop conf = do
     userSays <- getInputLine "core >>> "
     maybe
        (return ())
        (\expr ->
            maybe
            (   liftIO
                    (parseEvalText (_compiler conf) (_run conf)
                    (TIO.putStrLn . last) (pack expr))
                >> loop conf)
            ($ conf)
            (lookup (head $ words expr) replFuncs)
        ) userSays

time action = do
    a <- liftIO getCPUTime
    res <- action
    b <- liftIO getCPUTime
    outputStrLn $ "time: " ++ show (fromIntegral (b - a) / 1e12)
    return res

replFuncs =
    [ (".?",    \conf -> outputStrLn helpStr >> loop conf)
    , (".help", \conf -> liftIO (TIO.putStrLn (T.unlines primitives)) >> loop conf)
    , (".toggletime", \conf -> do
        outputStrLn $ "timing enabled: " ++ show (not (_timeIt conf))
        loop $ conf{ _timeIt = not $ _timeIt conf })
    , (".quit", \conf -> outputStrLn "au revoir")
    ]

helpStr = "Type a core expression or a REPL command: " ++ unwords (map fst replFuncs)


