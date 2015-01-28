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

timMk1 :: Compiler
timMk1 = Compiler
    "timMk1"
    (compileMk1 >=> evalMk1 >=> return .
    map (\st -> defaultState
        { output = showResultMk1 st
        , statistics = pack $ show st }))

data TimStateMk1 = TS
        { _instr :: [Instr], _frame :: FramePtr, _stack :: [Closure]
        , _valueStack :: ValueStack, _dump :: Dump -- not used in Mk1
        , _heap :: Heap Frame, _codestore :: [(Text, [Instr])]
        , _statistics :: Statistics
        } deriving (Eq)

instance Show TimStateMk1 where
    show (TS instr frame stack valuestack dump heap codestore stats) =
        "stack: " ++ show stack
     -- ++ "\nstack references: " ++ showStack heap stack
     ++ "\ndump: " ++ show dump
     ++ "\nvstack: " ++ show valuestack
     ++ "\nheap: " ++ show heap -- showHeap' heap
     ++ "\ncurrent frame:" ++ show frame
     ++ "\ncurrent instructions: " ++ show instr
     ++ "\ncode store: " ++ show codestore
     ++ "\nstats: " ++ show stats ++"\n"



type Frame = [Closure]

data Instr = Take Int
           | Enter AMode
           | Push AMode
           deriving (Eq, Show)

data AMode = Arg Int
           | Label Text
           | Code [Instr]
           | IntConst Int
           deriving (Eq, Show)

data Closure = Closure [Instr] FramePtr
            deriving (Eq, Show)

data FramePtr = FrameAddr Addr
              | FrameInt Int
              | FrameNull
              deriving (Show, Eq)

fromFrameAddr (FrameAddr a) = return a
fromFrameAddr x = throwError $ "expected frame address, received: " ++ show x

data ValueStack = ValueStack
            deriving (Eq, Show)

data Dump = Dump
            deriving (Eq, Show)

data Statistics = Stats { _steps :: Int, _heapstats :: HStats }
            deriving (Eq, Show)
initStatistics = Stats 0 initHStats

showResultMk1 = pack . show . _frame

evalMk1 state = do
    rest_states <- if isFinal state then return [] else
      do
        next_state <- takeStep state
        evalMk1 $ next_state
      `catchError` (\e -> throwError $ "STATE TRACE\n" ++ show state ++ "\n\n" ++ e)
    return (state : rest_states)
    where
          isFinal = (==[]) . _instr

takeStep state = liftM admin $ step (head $ _instr state) state{_instr = tail $ _instr state}
    where
     admin st@(TS{_statistics = stats}) = st{_statistics =
       stats{_steps = _steps stats + 1, _heapstats = H._hstats $ _heap st}}

{-

State transition description of the evaluation of each instruction

See 2.2 State Transition Systems in Implementing Functional Languages:
a tutorial by Simon Peyton Jones, but these are fairly easy to read as long
as you know functional linked lists ([], :)


Take n points the program to a new frame of size n

---------------------------------------------------------------
Take n:i frame  c1 : ... : cn : stk heap                      c
       i frame'                 stk heap[f' -> [c1, ..., cn]] c
---------------------------------------------------------------


Push fetches the nth closure from the frame and pushes it onto the stack

---------------------------------------------------------------
Push (Arg k):i frame          stk heap[f -> (i1, f1), ..., (ik, fk), ..., (in, fn)] c
             i frame (ik, fk):stk heap                                              c

Push (Label l):i frame                   stk heap c[l -> newinstr]
               i frame (newinstr, frame):stk heap c

Push (Code newinstr):i frame                   stk heap c
                     i frame (newinstr, frame):stk heap c

Push (IntConst n):i frame             stk heap c
                  i frame ([], Int n):stk heap c
---------------------------------------------------------------
Notice IntConst makes a new closure with an empty instruction sequence, this
is basically the halt command to this machine. All the Mk1 can really do is
print single integers or loop forever...


Enter relates directly to Push. It always changes the machine's current code.
---------------------------------------------------------------
[Enter (Label l)] frame stk heap c[l -> i]
                i frame stk heap c


[Enter (Arg k)] frame stk heap[f -> (i1, f1), ..., (ik, fk), ..., (in, fn)] c
             ik fk    stk heap

[Enter (Code i)] frame stk heap c
               i frame stk heap c

[Enter (IntConst n)] frame   stk heap c
                  [] (Int n) stk heap c
-}

step (Take n) state
    | length (_stack state) < n = throwError "too few args for Take instruction"
    | otherwise = return state{ _heap = newheap, _frame = FrameAddr newframe, _stack = drop n $ _stack state }
    where (newheap, newframe) = H.alloc (take n $ _stack state) (_heap state)

step (Enter mode) st = do
    (Closure instr' frame') <- modeToClosure mode (_frame st) (_heap st) (_codestore st)
    return st { _instr = instr', _frame = frame' }

step (Push mode) st  = do
    closure <- modeToClosure mode (_frame st) (_heap st) (_codestore st)
    return st { _stack = closure : _stack st }

modeToClosure (Arg n)      frame heap codestore = do
    addr <- fromFrameAddr frame
    frames <- H.lookup addr heap
    return $ frames !! (n - 1)
modeToClosure (Code il)    frame heap codestore = return $ Closure il frame
modeToClosure (Label l)    frame heap codestore = do
    code <- maybe (throwError $ "label not found: " ++ unpack l) return
        $ Prelude.lookup l codestore
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

type Env = [(Text, AMode)]

compileSC :: Env -> Supercombo Text -> ThrowsError (Text, [Instr])
compileSC env (Supercombo name args e) = do
    let newEnv = env ++ zip args (map Arg [1..])
    instr <- compileR e newEnv
    return (name, if null args then instr else Take (length args):instr)


compileR :: Expr Text -> Env -> ThrowsError [Instr]
compileR (App e1 e2) env = do
    x <- compileA e2 env
    f <- compileR e1 env
    return $ Push x : f
compileR e@(Var _)   env = compileA e env >>= \var -> return [Enter var]
compileR e@(Num _)   env = compileA e env >>= \num -> return [Enter num]
compileR e           _ = throwError $ "cannot (yet) compile expression: " ++ show e


compileA :: Expr Text -> Env -> ThrowsError AMode
compileA (Var v)     env =
    maybe (throwError $ "unknown variable: " ++ show v) return
    $ Prelude.lookup v env
compileA (Num n)     _ = return $ IntConst n
compileA e           env = liftM Code $ compileR e env
