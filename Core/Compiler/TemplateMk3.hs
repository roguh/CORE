{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Core.Compiler.TemplateMk3
--(compile, eval, showResults, runCore)
where

import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as TIO

import Control.Monad
import Control.Monad.Error

import Util

import Core.Parser
import Core.Types
import Core.PrettyPrint

import Core.Util.Heap as H
import Core.Util.Prelude

import Core.Compiler

templateMk3 :: Compiler
templateMk3 = Compiler
    "Template Instantiator Mk3"
    (compileMk3 >=> evalMk3 >=> return .
    map (\st -> State
        (pack $ either ("BAD ADDR AT TOP OF STACK "++) show $
            H.lookup (head $ _stack st) (_heap st))
        (pack $ showState st)))

showState (TS stk dump heap globs stats) =
   "stack: " ++ show stk ++
    "\ndump: " ++ show dump ++
    "\nglobs: " ++ show globs ++
    "\nsteps taken: " ++ show stats ++
    "\n" ++ showHeap heap






data Node = NApp Addr Addr
          | NSupercombo Text [Text] (Expr Text)
          | NNum Int
          | NInd Addr
          deriving (Show, Eq)

data TiStateMk3 = TS { _stack :: [Addr], _dump :: ()
                  , _heap :: Heap Node, _globals :: [(Text, Addr)]
                  , _stats :: Stats }
                deriving (Show, Eq)

data Config = Config { printStepTrace :: Bool }

data Stats = Stats
    { _heapStats :: H.HStats, _steps :: Int, _primReds :: Int
    , _superReds :: Int, _maxStackDepth :: Int }
    deriving (Eq)

instance Show Stats where
    show (Stats hstats steps primRed superRed maxDepth) =
            "steps: " ++ show steps
         ++ ", heap stats: " ++ showHStats hstats
         ++ ", prim reds: " ++ show primRed
         ++ ", super reds: " ++ show superRed
         ++ ", max stack: " ++ show maxDepth

initStats = Stats H.initHStats 0 0 0 0

isFinal (TS {_stack = [soleAddr], _heap = h}) = liftM isDataNode $ H.lookup soleAddr h
isFinal (TS {_stack = []}) = throwError "empty stack!"
isFinal _ = return False

isDataNode (NNum _) = True
isDataNode _ = False

initDump = ()

compileMk3 :: CoreProgram -> ThrowsError TiStateMk3
compileMk3 program = do
    let supercombos = prelude ++ program
    let (iHeap, globals) = buildInitialHeap supercombos
    addrMain <- maybeToEither "main undefined" $ Prelude.lookup "main" globals
    return $ TS [addrMain] initDump iHeap globals initStats

buildInitialHeap = mapAccuml allocateSupercombo H.init

allocateSupercombo heap (Supercombo name bs bod) =
    let (heap', addr) = H.alloc (NSupercombo name bs bod) heap
    in (heap', (name, addr))

doAdmin oldstate newstate
    = newstate{_stats = newStats}
    where newStats = oldstats
           { _steps = _steps oldstats + 1
           , _heapStats = _hstats $ _heap oldstate
           -- , _primReds
           -- , _superReds = _superReds oldstats + 1
           , _maxStackDepth = if stackDepth > _maxStackDepth oldstats
                              then stackDepth else _maxStackDepth oldstats }
          stackDepth = Prelude.length $ _stack oldstate
          oldstats = _stats oldstate

evalMk3 :: TiStateMk3 -> ThrowsError [TiStateMk3]
evalMk3 = evalMk3With (Config False)

evalMk3With cfg state = do
    isFin <- isFinal state
    rest <- -- step trace in case of error
        (if not $ printStepTrace cfg then id else
        flip catchError (\e -> throwError $ showState state ++ "\n\n" ++ e))
            (if isFin
            then return []
            else liftM (doAdmin state) (step state) >>= evalMk3With cfg)
    return (state : rest)

step :: TiStateMk3 -> ThrowsError TiStateMk3
step ts = H.lookup (Prelude.head $ _stack ts) (_heap ts) >>= dispatch
    where
        dispatch (NNum _) = throwError "number applied as a function"
        dispatch (NApp f x) = appStep ts f x
        dispatch (NSupercombo name args bod) = supercomboStep ts name args bod
        dispatch (NInd addr) = return $ ts { _stack = addr:Prelude.tail (_stack ts) }

-- unwind onto the stack
appStep ts@(TS {_stack = stack}) f x = return $ ts {_stack = f:stack}

-- instantiate the body, bind arguments to argument addresses from stack
-- then discard arguments and the root from the stack
-- push root of the result onto the stack
-- finally, update the root of the redex with the result
supercomboStep ts _ argNames bod = do
    let nArgs = (Prelude.length argNames)
        rootAddr = _stack ts !! nArgs
        newStack = Prelude.drop (nArgs + 1) (_stack ts)
    args <- getargs nArgs (_heap ts) (_stack ts)
    let argBindings = Prelude.zip argNames args
        env = argBindings ++ _globals ts
        (heapAdd, isVar) = case bod of
            (Var _) -> (alloc, True)
            _ -> (H.update rootAddr, False)
    (newHeap', resultAddr) <- instantiate heapAdd (_heap ts) env bod
    let newHeap = if isVar then fst $ H.update rootAddr (NInd resultAddr) newHeap'
                           else newHeap'
    return $ ts{ _stack = resultAddr:newStack, _heap = newHeap }

getargs n heap (sc:stack) = do
    when (n > Prelude.length stack)
        (do (NSupercombo name _ _) <- H.lookup sc heap
            throwError $ "supercombinator \"" ++ unpack name
                      ++ "\" expected " ++ show n
                      ++ " args, received " ++ show (Prelude.length stack))
    mapM getArg $ Prelude.take n stack
    where getArg addr = do (NApp _ arg) <- H.lookup addr heap
                           return arg

instantiate :: (Node -> Heap Node -> (Heap Node, Addr)) -> Heap Node -> [(Text, Addr)] -> Expr Text -> ThrowsError (Heap Node, Addr)
instantiate heapAdd heap _ (Num n) = return $ heapAdd (NNum n) heap

instantiate _ heap env (Var v) = do
    val <- maybeToEither ("undefined variable: " ++ unpack v) (Prelude.lookup v env)
    return (heap, val)

instantiate heapAdd heap env (App f x) = do
    (heap1, fAddr) <- instantiate alloc heap env f
    (heap2, xAddr) <- instantiate alloc heap1 env x
    return $ heapAdd (NApp fAddr xAddr) heap2

instantiate heapAdd heap env (Let isRec defs expr)
  | not isRec = do
         (newheap, newenv) <- foldM (\(currentheap, currentenv) (name, subexpr) ->
              do (newheap', addr) <- instantiate alloc currentheap env subexpr
                 return (newheap', (name, addr):currentenv))
             (heap, env) defs
         instantiate heapAdd newheap newenv expr
  | otherwise = throwError "cannot deal with letrec"

instantiate heapAdd _ _ (Constructor _ _) = throwError "cannot deal with constructors"

instantiate heapAdd _ _ (Case _ _) = throwError "cannot deal with case expressions"

instantiate heapAdd _ _ (Lambda _ _) = throwError "cannot deal with lambdas"

