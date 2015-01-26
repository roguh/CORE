module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T (unlines, pack, unpack, null)
import Data.List (isPrefixOf, intercalate)

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

import Core.Compiler.TemplateMk1
import Core.Compiler.TemplateMk3
import Core.Compiler.GMachineMk1
import Core.Compiler.GMachineMk2
import Core.Compiler.GMachineMk3
import Core.Compiler.GMachineMk4
import Core.Compiler.GMachineMk5
import Core.Compiler.GMachineMk6
import Core.Compiler.GMachineMk7
import Core.Compiler.TimMk1

import Core.Compiler
import Core.Util.Prelude

compilers = [templateMk1, templateMk3
    , gmachineMk1, gmachineMk2, gmachineMk3, gmachineMk4
    , gmachineMk5, gmachineMk6, gmachineMk7
    , timMk1
    ]

data REPLConfig = Conf { _compiler :: Compiler, _run :: RunCore, _timeIt :: Bool }
defaultConfig = Conf gmachineMk6 run False

main :: IO ()
main =
    do
        hSetBuffering stdin NoBuffering
        as <- getArgs
        case length as of
            _ -> do putStrLn helpStr
                    runInputT haskelineSettings (loop defaultConfig)
            -- must add file processing...
            -- _ -> return ()
     where
        evalFile conf fname = catchIOError
                (parseEvalFile (_compiler conf) (_run conf) (TIO.putStrLn . last) fname)
                (putStrLn . (("failure reading " ++ fname ++ "\n")++) . show)

        coreCompletions word = return $
            let reps = filter (isPrefixOf word) $
                        map unpack primitives ++ map fst replFuncs
            in flip map reps $ \rep -> Completion { replacement = rep
                                 , display = rep, isFinished = rep == word }
        haskelineSettings = flip setComplete defaultSettings $ \line -> do
            (_, cs1) <- completeWord Nothing " \t\n\r\f\v" coreCompletions line
            (s, cs2) <- completeFilename line
            return (s, cs1 ++ cs2)

primitives = map _name prelude

loop conf = do
     userSays <- getInputLine "core >>> "
     maybe
        (return ())
        (\expr ->
            maybe
            (  liftIO
                    (parseEvalText (_compiler conf) (_run conf)
                    ((if _timeIt conf then time else id) . TIO.putStrLn . last) (pack expr))
                >> loop conf)
            (\f -> f conf (tail $ words expr))
            (lookup (head $ words expr) replFuncs)
        ) userSays

time action = do
    a <- getCPUTime
    res <- action
    b <- getCPUTime
    putStrLn $ "time: " ++ show (fromIntegral (b - a) / 1e12)
    return res

replFuncs =
    [ (".?",    \conf _ -> outputStrLn helpStr >> loop conf)
    , (".help", \conf _ -> liftIO (TIO.putStrLn (T.unlines primitives)) >> loop conf)
    , (".compiler", \conf args ->
        let help = outputStrLn $ "if in doubt, use gmachinemk6. compilers available: " ++
                intercalate ", " (map (unpack . compilerName) compilers)
        in case args of
            [] -> help >> loop conf
            (x:_) -> maybe (help >> loop conf)
                (\newcomp -> do
                    outputStrLn ("changing compiler to " ++ x)
                    loop $ conf { _compiler = newcomp})
                $ lookup (pack x) (map (\c -> (compilerName c, c)) compilers))
    , (".debugon", \conf _ -> do
        outputStrLn "debugging enabled, use .debugoff to disable"
        loop $ conf{ _run = runTest })
    , (".debugoff", \conf _ -> do
        outputStrLn "debugging disabled, use .debugon to enable"
        loop $ conf{ _run = run })
    , (".toggletime", \conf _ -> do
        outputStrLn $ "timing enabled: " ++ show (not (_timeIt conf))
        loop $ conf{ _timeIt = not $ _timeIt conf })
    , (".quit", \conf _ -> outputStrLn "au revoir")
    ]

helpStr = "Type a core expression or a REPL command: " ++ unwords (map fst replFuncs)


