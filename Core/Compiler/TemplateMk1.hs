{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Core.Compiler.TemplateMk1
--(compile, eval, showResults, runCore)
where

import Data.Text as Text
import qualified Data.Text.IO as TIO

import Control.Monad
import Control.Monad.Error

import Util

import Core.Types
import Core.PrettyPrint

import Core.Util.Heap as H
import Core.Util.Prelude

import Core.Compiler

templateMk1 :: TiStateMk1
templateMk1 = undefined

instance CoreCompiler TiStateMk1 where
    compile = compileMk1
    eval = evalMk1
    showStateTrace = showResultsMk1

data Node = NApp Addr Addr
          | NSupercombo Text [Text] (Expr Text)
          | NNum Int
          deriving (Show, Eq)

data TiStateMk1 = TS { _stack :: [Addr], _dump :: ()
                  , _heap :: Heap Node, _globals :: [(Text, Addr)]
                  , _stats :: Stats }
                deriving (Show, Eq)

data Stats = Stats
    { _heapStats :: HStats, _steps :: Int, _primReds :: Int
    , _superReds :: Int, _maxStackDepth :: Int }
    deriving (Eq)

instance Show Stats where
    show (Stats hstats steps primRed superRed maxDepth) =
            "steps: " ++ show steps
         ++ ", heap allocs: " ++ showHStats hstats
         ++ ", prim reds: " ++ show primRed
         ++ ", super reds: " ++ show superRed
         ++ ", max stack: " ++ show maxDepth

data Config = Config { printStepTrace :: Bool }

isFinal (TS {_stack = [soleAddr], _heap = h}) = liftM isDataNode $ H.lookup soleAddr h
isFinal (TS {_stack = []}) = throwError "empty stack!"
isFinal _ = return False

isDataNode (NNum _) = True
isDataNode _ = False

initStats = Stats initHStats 0 0 0 0

initDump = ()

compileMk1 :: CoreProgram -> ThrowsError TiStateMk1
compileMk1 program = do
    let supercombos = prelude ++ program
    let (iHeap, globals) = buildInitialHeap supercombos
    addrMain <- maybeToEither "main undefined" $ Prelude.lookup "main" globals
    return $ TS [addrMain] initDump iHeap globals initStats

buildInitialHeap = mapAccuml allocateSupercombo H.init

showResultsMk1 :: [TiStateMk1] -> Text
showResultsMk1 [] = "no results!"
showResultsMk1 ts = Text.concat (Prelude.map (\s -> pack (showResult' s) `append` "\n\n") ts)
             `append` "final result: "
             `append` either (pack . show) (pack . show)
                (H.lookup (Prelude.head $ _stack final) (_heap final))
    where   final = Prelude.last ts

showResult' (TS stk dump heap globs stats) =
   "stack: " ++ show stk ++
    "\ndump: " ++ show dump ++
    "\nglobs: " ++ show globs ++
    "\nsteps taken: " ++ show stats ++
    "\n" ++ showHeap heap

allocateSupercombo heap (Supercombo name bs bod) =
    let (heap', addr) = H.alloc (NSupercombo name bs bod) heap
    in (heap', (name, addr))

doAdmin oldstate newstate
    = newstate{_stats = newStats}
    where newStats = oldstats
           { _steps = _steps oldstats + 1
           , _heapStats = _hstats $ _heap oldstate
           -- , _allocs = H._nAllocs $ _heap oldstate
           -- , _primReds
           -- , _superReds = _superReds oldstats + 1
           , _maxStackDepth = if stackDepth > _maxStackDepth oldstats
                              then stackDepth else _maxStackDepth oldstats }
          stackDepth = Prelude.length $ _stack oldstate
          oldstats = _stats oldstate

evalMk1 :: TiStateMk1 -> ThrowsError [TiStateMk1]
evalMk1 = evalMk1With (Config False)

evalMk1With cfg state = do
    isFin <- isFinal state
    rest <- -- step trace in case of error
        (if not $ printStepTrace cfg then id else
        flip catchError (\e -> throwError $ showResult' state ++ "\n\n" ++ e))
            (if isFin
            then return []
            else liftM (doAdmin state) (step state) >>= evalMk1With cfg)
    return (state : rest)

step :: TiStateMk1 -> ThrowsError TiStateMk1
step ts = H.lookup (Prelude.head $ _stack ts) (_heap ts) >>= dispatch
    where
        dispatch (NNum _) = throwError "number applied as a function"
        dispatch (NApp f x) = appStep ts f x
        dispatch (NSupercombo name args bod) = supercomboStep ts name args bod

-- unwind onto the stack
appStep ts@(TS {_stack = stack}) f x = return $ ts {_stack = f:stack}

-- instantiate the body, bind arguments to argument addresses from stack
-- then discard arguments and the root from the stack
-- and push root of the result onto the stack
-- (no updates yet)
supercomboStep ts name argNames bod = do
    args <- getargs (Prelude.length argNames) (_heap ts) (_stack ts)
    let argBindings = Prelude.zip argNames args
        env = argBindings ++ _globals ts
    (newHeap, resultAddr) <- instantiate bod (_heap ts) env
    let newStack = resultAddr : Prelude.drop (Prelude.length argNames + 1) (_stack ts)
    return $ ts{ _stack = newStack, _heap = newHeap }

getargs n heap (sc:stack) = do
    when (n > Prelude.length stack)
        (do (NSupercombo name _ _) <- H.lookup sc heap
            throwError $ "supercombinator \"" ++ unpack name
                      ++ "\" expected " ++ show n
                      ++ " args, received " ++ show (Prelude.length stack))
    mapM getArg $ Prelude.take n stack
    where getArg addr = do (NApp _ arg) <- H.lookup addr heap
                           return arg

instantiate :: Expr Text -> Heap Node -> [(Text, Addr)] -> ThrowsError (Heap Node, Addr)

instantiate (Num n) heap _ = return $ alloc (NNum n) heap

instantiate (Var v) heap env = do
    val <- maybeToEither ("undefined variable: " ++ unpack v) (Prelude.lookup v env)
    return (heap, val)

instantiate (App f x) heap env = do
    (heap1, fAddr) <- instantiate f heap env
    (heap2, xAddr) <- instantiate x heap1 env
    return $ alloc (NApp fAddr xAddr) heap2

instantiate (Let isRec defs expr) heap env
  | not isRec = do
         (newheap, newenv) <- foldM (\(currentheap, currentenv) (name, subexpr) ->
              do (newheap', addr) <- instantiate subexpr currentheap env
                 return (newheap', (name, addr):currentenv))
             (heap, env) defs
         instantiate expr newheap newenv
  | otherwise = throwError "cannot deal with letrec"

instantiate (Constructor _ _) heap env = throwError "cannot deal with constructors"

instantiate (Case _ _) heap env = throwError "cannot deal with case expressions"

instantiate (Lambda _ _) heap env = throwError "cannot deal with lambdas"

