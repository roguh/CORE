{-# LANGUAGE OverloadedStrings #-}
module Core.Compiler.GMachineMk1
--(compile, eval, showResults, runCore)
where

import Control.Monad
import Control.Monad.Error

import Data.Text (Text, pack, unpack)
import Data.Maybe

import Util

import Core.Types

import Core.Util.Heap as H
import Core.Util.Prelude

import Core.Compiler

gmachineMk1 :: Compiler
gmachineMk1 = Compiler
    "gmachinemk1"
    (compileMk1 >=> evalMk1 >=> return .
    map (\st -> defaultState
        { output = showResultMk1 st
        , statistics = pack $ show st }))

showResultMk1 = pack . either id show . (\st -> H.lookup (head $ _stack st) (_heap st))

data GMStateMk1 = GMS { _code :: [Instruction], _stack :: [Addr]
                   , _heap :: Heap Node    , _globals :: [(Text, Addr)]
                   , _statistics :: (Int, HStats) }
                   deriving (Eq)

instance Show GMStateMk1 where
    show (GMS code stack heap globals stats) =
        "stack: " ++ show stack
     ++ "\nheap: " ++ show heap
     ++ "\ncode: " ++ show code
     ++ "\nglobals: " ++ show globals
     ++ "\nstats: " ++ show stats

data Instruction = Pushglobal Text
                 | Pushint Int
                 | Push Int
                 | Mkap
                 | Slide Int
                 | Unwind
                 deriving (Eq, Show)

data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Int [Instruction]
          deriving (Eq, Show)

evalMk1 :: GMStateMk1 -> ThrowsError [GMStateMk1]
evalMk1 state = do
    restStates <- if isFinal state then return []
                  else (liftM doAdmin $ step state) >>= evalMk1
    return $ state : restStates

doAdmin st@(GMS { _statistics = (steps, hstats), _heap = h }) =
    st { _statistics = (steps + 1, _hstats h) }

isFinal = (==[]) . _code

step :: GMStateMk1 -> ThrowsError GMStateMk1
step state = dispatch i (state{ _code = is })
    where (i:is) = _code state

dispatch :: Instruction -> GMStateMk1 -> ThrowsError GMStateMk1

dispatch (Pushglobal f) state = do
    a <- maybeToEither ("undeclared global: " ++ unpack f)
       $ Prelude.lookup f (_globals state)
    return $ state{ _stack = a:_stack state }

dispatch (Pushint n)    state
--  = return $ state { _heap = heap', _stack = a:_stack state}
--    where (heap', a) = H.alloc (NNum n) (_heap state)
    | isJust allocd = return $ state{ _stack = fromJust allocd : _stack state }
    | otherwise = return $ state{ _heap = heap', _stack = a:_stack state, _globals = (pack $ show n,a):_globals state }
  where (heap', a) = H.alloc (NNum n) (_heap state)
        allocd = Prelude.lookup (pack $ show n) (_globals state)


dispatch (Push n)       state = do
    let stk = _stack state
    when (length stk < n+2)
         (throwError $ "stack of size " ++ show (length stk) ++
                       " needs to have at least n+2 elements to push n=" ++ show n)
    ap_ <- H.lookup (stk !! (n + 1)) (_heap state)
    ap_arg <- case ap_ of
        NAp _ a2 -> return a2
        ns -> throwError $ "wtf, should find a NAp node on push, found: " ++ show ns
    return $ state{ _stack = ap_arg : stk }

dispatch (Slide n)      state = do
    when (length (_stack state) < n+1)
         (throwError $ "stack needs to have at n+1 elements to slide n=" ++ show n)
    let (a:stk) = _stack state
    return $ state{ _stack = a:drop n stk }

dispatch Mkap           state = do
    let stk = _stack state
    when (length stk < 2) (throwError "not enough arguments on the stack")
    let (a1:a2:stk') = stk
        (heap', a) = H.alloc (NAp a1 a2) (_heap state)
    return $ state{ _heap = heap', _stack = a:stk' }

dispatch Unwind         state = do
    when (null $ _stack state) (throwError "cannot unwind with an empty stack")
    let (a:as) = _stack state
        newState (NNum n) = return state
        newState (NAp a1 a2) = return $ state{ _code = [Unwind], _stack = a1:a:as }
        newState (NGlobal nargs code)
            | length as < nargs = throwError "unwinding with too few arguments"
            | otherwise = return $ state{ _code = code }
    top <- H.lookup a (_heap state)
    newState top






data GMCompiledSC = GMCompiledSC
    { _scname :: Text, _argNum :: Int, _sccode :: [ Instruction ] }

compileMk1 :: CoreProgram -> ThrowsError GMStateMk1
compileMk1 program = do
    (heap, globals) <- do
        compiled <- compiledM
        return $ mapAccuml allocateSupercombo H.init compiled
    return $ GMS initCode [] heap globals initStats
    where   initStats = (0, initHStats)
            initCode = [ Pushglobal "main", Unwind ]

            compiledM = mapM compileSupercombo $ prelude ++ program

            allocateSupercombo heap (GMCompiledSC name nargs ins) =
                (heap', (name, addr))
                where (heap', addr) = H.alloc (NGlobal nargs ins) heap




type GMCompiler = [(Text, Int)] -> Expr Text -> ThrowsError [Instruction]

compileSupercombo :: Supercombo Text -> ThrowsError GMCompiledSC
compileSupercombo (Supercombo name env bod) =
        compileR (zip env [0..]) bod
    >>= return . GMCompiledSC name (length env)

compileR :: GMCompiler
compileR env e = do
    ins <- compileC env e
    return $ ins ++ [Slide $ length env + 1, Unwind]

compileC :: GMCompiler
compileC _ (Num n) = return [Pushint n]
compileC env (Var v)
    | v `elem` map fst env = return [Push . fromJust $ Prelude.lookup v env]
    | otherwise = return [Pushglobal v]
compileC env (App e1 e2) = do
    e2C <- compileC env e2
    e1C <- compileC (argOffset 1 env) e1
    return $ e2C ++ e1C ++ [Mkap]
compileC _ _ = throwError "not implemented yet!"

argOffset n env = do
    (v, m) <- env
    return (v, n + m)
