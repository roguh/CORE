{-
The Three Instruction Machine is named for a simple subset of its instructions
Take, Push and Enter.
Main idea: when compiling, "flatten" each supercombinator into small chunks to
avoid any unneeded code traversals when evaluating.

Example:
f x y = g E1 E2

can be transformed into
f x y = g (c1 x y) (c2 x y)
c1 x y = E1
c2 x y = E2

where E1 and E2 are arbitrary expressions.
This transformation is applied to any subexpressions in E1 and E2. This
removes all nested structure from expressions, so it's called flattening.

What if c1 and c2 took many arguments, not just two? The transformation will
now involve tuples:
f x y = let args = (x,y)
        in g (c1 args) (c2 args)
c1 (x,y) = E1
c2 (x,y) = E2


This machine is spineless, unlike the G-Machine and the Template
Instantiation Machine. The arguments are not found along the edge of a graph,
but in a single (potentially shared) structure. During evaluation, the stack
will consist of closures, a closure is a sequence of instructions and a
frame

-}

{-# LANGUAGE OverloadedStrings #-}
module Core.Compiler.TimMk1
where

import Control.Monad
import Control.Monad.Error

import Data.Text (Text, pack, unpack, append)
import qualified Data.Text as Text (concat, unwords)
import Data.Maybe
import Data.List

import Util

import Core.Types

import Core.Parser (parseConstructor)
import Core.Util.Heap as H
import Core.Util.Prelude

import Core.Compiler

gmachineMk1 :: Compiler
gmachineMk1 = Compiler
    "timmk1"
    (compileMk1 >=> evalMk1 >=> return .
    map (\st -> defaultState
        { output = showResultMk1 st
        , statistics = pack $ show st }))

data TimStateMk1 = TS
        { _instr :: Instr, _frame :: FramePtr, _stack :: [Closure]
        , _valueStack :: ValueStack, _dump :: Dump -- not used in Mk1
        , _heap :: Heap Frame, _codestore :: [(Text, [Instr])]
        , _statistics :: Statistics
        }

type Frame = [Closure]

data Instr = Take Int
           | Enter AMode
           | Push AMode

data AMode = Arg Int
           | Label Text
           | Code [Instr]
           | IntConst Int

data Closure = Closure [Instr] FramePtr

data FramePtr = FrameAddr Addr
              | FrameInt Int
              | FrameNull
              deriving (Show, Eq)

fromFrameAddr (FrameAddr a) = a
fromFrameAddr x = throwError $ "expected frame address, received: " ++ show x

data ValueStack = ValueStack

data Dump = Dump

data Statistics = Stats { _steps :: Int, _heapstats :: HStats }
initStatistics = Stats 0 initHStats

showResultMk1 = undefined

evalMk1 state = return (state : rest_states)
    where rest_states | isFinal state = []
                      | otherwise = evalMk1 next_state
          next_state = admin $ step (head $ _instr state) state{_instr = tail $ _instruction state}
          admin st@(TS{_statistics = stats}) = st{_statistics =
            stats{_steps = _steps stats + 1, _heapstats = H._hstats $ _heap st}}
          isFinal = (==[]) . _instr

step (Take n) state
    | length (_stack state) < n = throwError "too few args for Take instruction"
    | otherwise = state{ _heap = newheap, _frame = FrameAddr newframe }
    where (newheap, newframe) = H.alloc (take n $ _stack state) (_heap state)

step (Enter mode) st = st { _instr = instr', _frame = frame' }
    where (Closure instr' frame') =
            modeToClosure mode (_frame st) (_heap st) (_codestore st)

step (Push mode) st  = st { _stack =
        modeToClosure mode (_frame st) (_heap st) (_codestore st): _stack st }

modeToClosure (Arg n)      frame heap codestore = do
    frames <- H.lookup frame heap
    return $ frames !! (n - 1)
modeToClosure (Code il)    frame heap codestore = return $ Closure il frame
modeToClosure (Label l)    frame heap codestore = do
    code <- maybe (throwError $ "label not found: " ++ l) return $ Prelude.lookup l codestore
    return $ Closure code frame
modeToClosure (IntConst n) frame heap codestore = return $ Closure intCode (FrameInt n)

-- no arithmetic yet!
intCode = []

{-

This simple TIM has three compilation schemes

SC[f x1 .. xn = e] env = Take n : R[e] bind env[x1 -> Arg 1, ... , xn -> Arg n]

R[e1 e2] env = Push (A[e2] env) : R[e1] env
R[a@(integer, variable, or supercombo)] = Enter (A[a] env)

A[x] env = env x
A[n] env = IntConst n
A[e] env = Code (R[e] env)

-}

compileMk1 program = do
    compiledSupercombos <- mapM (compileSC initEnv) supercombos
    let compiledCode = compiledSupercombos ++ compiledPrimitives
    return TS
        { _instr = [Enter $ Label "main"]
        , _frame = FrameNull, _stack = []
        , _valueStack = ValueStack, _dump = Dump -- not used in Mk1
        , _heap = H.init, _codestore = compiledCode
        , _statistics = initStatistics
        }
    where
        supercombos = prelude ++ program
        compiledPrimitives = []
        initEnv = [(name, Label name) | Supercombo name _ _ <- supercombos]
               ++ [(name, Label name) | (name, _) <- compiledPrimitives]

type CompilerEnv = [(Text, AMode)]

compileSC env (Supercombo name args e) =
    (name, Take (length args) : instr)
    where instr = compileR e newEnv
          newEnv = zip args (map Arg [1..]) ++ env


compileR (App e1 e2) env = do
    x <- compileA e2 env
    f <- compileR e1 env
    return $ Push x : f
compileR e@(Var v)   env = compileA e >>= \var -> return [Enter var]
compileR e@(Num n)   env = compileA e >>= \num -> return [Enter num]
compileR e           env = throwError $ "cannot (yet) compile expression: " ++ e


compileA (Var v)     env = return $ fromMaybe
    (throwError $ "unknown variable: " ++ show v) $ Prelude.lookup env v
compileA (Num n)     env = return $ IntConst n
compileA e           env = liftM Code $ compileR e env
