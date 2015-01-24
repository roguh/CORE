{-# LANGUAGE OverloadedStrings #-}
module Core.Compiler.GMachineMk3
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

gmachineMk3 :: GMStateMk3
gmachineMk3 = undefined

instance CoreCompiler GMStateMk3 where
    compile = compileMk3
    eval = evalMk3
    showStateTrace = showResultsMk3

data GMStateMk3 = GMS { _code :: [Instruction], _stack :: [Addr]
                   , _heap :: Heap Node    , _globals :: [(Text, Addr)]
                   , _statistics :: (Int, HStats)
                   , _oldcode :: [Instruction] }
                   deriving Eq

instance Show GMStateMk3 where
    show (GMS code stack heap globals stats oldcode) =
        "stack: " ++ show stack
     ++ "\nheap: " ++ showHeap heap
     ++ "\ncurrent code: " ++ show code
     ++ "\nall code: " ++ show (reverse oldcode)
     ++ "\nglobals: " ++ show globals
     ++ "\nstats: " ++ show stats

data Instruction = Pushglobal Text
                 | Pushint Int
                 | Push Int
                 | Alloc Int
                 | Update Int
                 | Pop Int
                 | Slide Int
                 | Mkap
                 | Unwind
                 deriving (Eq, Show)

data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Text Int [Instruction]
          | NInd Addr
          deriving (Eq, Show)

isIndirection (NInd _) = True
isIndirection _ = False

showResultsMk3 :: [GMStateMk3] -> Text
showResultsMk3 = pack . (\result ->
        show result ++ "\nfinal result: "
        ++ show (H.lookup (head $ _stack result) $ _heap result)) . last

evalMk3 :: GMStateMk3 -> ThrowsError [GMStateMk3]
evalMk3 state = do
    restStates <- if isFinal state then return []
                  else (liftM doAdmin $ step state) >>= evalMk3
    return $ state : restStates

doAdmin st@(GMS { _statistics = (steps, hstats), _heap = h }) =
    st { _statistics = (steps + 1, _hstats h) }

isFinal = (==[]) . _code

step :: GMStateMk3 -> ThrowsError GMStateMk3
step state = dispatch i (state{ _code = is, _oldcode = i:_oldcode state})
    where (i:is) = _code state

dispatch :: Instruction -> GMStateMk3 -> ThrowsError GMStateMk3

dispatch (Pushglobal f) state = do
    a <- maybeToEither ("undeclared global: " ++ unpack f)
       $ Prelude.lookup f (_globals state)
    return $ state{ _stack = a:_stack state }

dispatch (Pushint n)    state
    | isJust allocd = return $ state{ _stack = fromJust allocd : _stack state }
    | otherwise = return $ state{ _heap = heap', _stack = a:_stack state, _globals = (pack $ show n,a):_globals state }
  where (heap', a) = H.alloc (NNum n) (_heap state)
        allocd = Prelude.lookup (pack $ show n) (_globals state)


dispatch (Push n)       state = do
    let stk = _stack state
    return $ state{ _stack = stk !! n : stk }

dispatch (Pop n)        state = return $ state{ _stack = drop n $ _stack state}

dispatch (Update n)     state = do
    let (heap', _) = H.update (stk !! n) (NInd a) (_heap state)
        (a:stk) = _stack state
    return state{ _stack = stk, _heap = heap' }

--TODO correct to use Mk1's?
dispatch (Slide n)      state = do
    when (length (_stack state) < n+1)
         (throwError $ "stack needs to have at n+1 elements to slide n=" ++ show n)
    let (a:stk) = _stack state
    return $ state{ _stack = a:drop n stk }

dispatch (Alloc n)      state =
    let (heap', emptyAddrs) =
            mapAccuml (const . H.alloc (NInd nullAddr)) (_heap state) [1..n]
    in return $ state{ _heap = heap', _stack = emptyAddrs ++ _stack state }

dispatch Mkap           state = do
    let (a1:a2:stk) = _stack state
        (heap', a) = H.alloc (NAp a1 a2) (_heap state)
    return $ state{ _heap = heap', _stack = a:stk }

dispatch Unwind         state
  | Right True == (fmap isIndirection . (`H.lookup` _heap state) . head . _stack) state = do
        aInd <- H.lookup (head $ _stack state) (_heap state)
        (NInd a) <- return aInd
        dispatch Unwind $ state{ _stack = a:tail (_stack state) }
  | otherwise = do
        when (null $ _stack state)
           $  throwError "cannot unwind with an empty stack"
        top <- H.lookup (head $ _stack state) (_heap state)
        case top of
          NGlobal name n c-> do
                newstk <- rearrangeStack name n (_heap state) (_stack state)
                return $ state{ _stack = newstk, _code = c }
          _ -> do
            let (a:as) = _stack state
                newState (NNum _) = return state
                newState (NAp a1 _) = return $
                    state{ _code = [Unwind], _stack = a1:a:as }
                newState _ = error "impossible"
            newState top




rearrangeStack name n heap stk = do
    when (length stk - 1 < n)
       $ throwError $ "unwinding global " ++ unpack name ++ " with too few arguments"
    stk' <- mapM (flip H.lookup heap >=> getArg) (tail stk)
    return $ take n stk' ++ drop n stk
 where
    getArg (NAp _ a2) = return a2
    getArg x = throwError $ "expected NAp node in stack, found: " ++ show x




data GMCompiledSC = GMCompiledSC
    { _scname :: Text, _argNum :: Int, _sccode :: [ Instruction ] }

compileMk3 :: CoreProgram -> ThrowsError GMStateMk3
compileMk3 program = do
    (heap, globals) <- do
        compiled <- compiledM
        return $ mapAccuml allocateSupercombo H.init compiled
    return $ GMS initCode [] heap globals initStats []
    where   initStats = (0, initHStats)
            initCode = [ Pushglobal "main", Unwind ]

            compiledM = mapM compileSupercombo $ prelude ++ program

            allocateSupercombo heap (GMCompiledSC name nargs ins) =
                (heap', (name, addr))
                where (heap', addr) = H.alloc (NGlobal name nargs ins) heap




type GMCompiler = [(Text, Int)] -> Expr Text -> ThrowsError [Instruction]

compileSupercombo :: Supercombo Text -> ThrowsError GMCompiledSC
compileSupercombo (Supercombo name env bod) =
    liftM (GMCompiledSC name (length env))
    $   compileR (length env) (zip env [0..]) bod

compileR :: Int -> GMCompiler
compileR arity env e = do
    ins <- compileC env e
    return $ ins ++ [Update arity, Pop arity, Unwind]

compileC :: GMCompiler
compileC _ (Num n) = return [Pushint n]
compileC env (Var v)
    | v `elem` map fst env = return [Push . fromJust $ Prelude.lookup v env]
    | otherwise = return [Pushglobal v]
compileC env (App e1 e2) = do
    e2C <- compileC env e2
    e1C <- compileC (argOffset 1 env) e1
    return $ e2C ++ e1C ++ [Mkap]
compileC env (Let recursive defs expr)
    | recursive = do
        (defsCode, _) <- foldM (\(acc, nth) (_, def) -> do
                code <- compileC env' def
                return (acc ++ code ++ [Update nth], nth - 1)
            ) ([], len - 1) defs
        exprCode <- compileC env' expr
        return $ [Alloc len] ++ defsCode ++ exprCode ++ [Slide len]
    | otherwise = do
        (defsCode, _) <- foldM (\(acc, offsetEnv) (_, def) -> do
                code <- compileC offsetEnv def
                return (acc ++ code, argOffset 1 offsetEnv)
            ) ([], env) defs
        exprCode <- compileC env' expr
        return $ defsCode ++ exprCode ++ [Slide len]
  where len = length defs
        env' = zip (map fst defs) [len-1, len-2 .. 0] ++ argOffset len env

compileC _ _ = throwError "not implemented yet!"

argOffset n env = do
    (v, m) <- env
    return (v, n + m)
