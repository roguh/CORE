{-# LANGUAGE OverloadedStrings #-}
module Core.Compiler.GMachineMk5
--(compile, eval, showResults, runCore)
where

import Control.Monad
import Control.Applicative
import Control.Monad.Error

import Data.Text (Text, pack, unpack, append)
import Data.Maybe

import Util

import Core.Types

import Core.Util.Heap as H
import Core.Util.Prelude

import Core.Compiler

gmachineMk5 :: Compiler
gmachineMk5 = Compiler
    "gmachinemk5"
    (compileMk5 >=> evalMk5 >=> return .
    map (\st -> defaultState
        { output = showResultMk5 st
        , statistics = pack $ show st }))

showResultMk5 = pack . either id show . getStackTop

data GMStateMk5 = GMS { _code :: [Instruction], _stack :: [Addr]
                    , _dump :: [([Instruction], [Addr])]
                    , _heap :: Heap Node    , _globals :: [(Text, Addr)]
                    , _statistics :: (Int, HStats)
                    , _oldcode :: [Instruction] }
                    deriving Eq

instance Show GMStateMk5 where
    show (GMS code stack dump heap globals stats oldcode) =
        "stack: " ++ show stack
     ++ "\nstack references: " ++ showStack heap stack
     ++ "\ndump: " ++ show dump
     ++ "\nheap: " ++ showHeap' heap
     ++ "\ncurrent code: " ++ show code
     ++ "\nall code: " ++ show (reverse oldcode)
     ++ "\nglobals: " ++ show globals
     ++ "\nstats: " ++ show stats

showHeap' (Heap _ sz free cts) = "heap of size " ++ show sz
    ++ "\nsome free addr: "
    ++ show (take 5 free)
    ++ "\nbindings: "
    ++ concatMap (\binding -> (if isGlobal $ snd binding then "\n\t\t" else "\n\t")
           ++ show binding) cts


showStack heap = ("[ "++) . (++"]") . concatMap ((++", ") . deepLookup heap)

deepLookup heap = go
    where go a = case H.lookup a heap of
            Right (NInd addr') -> "NInd (" ++ go addr' ++ ")"
            Right (NAp a1 a2) -> "(" ++ go a1 ++ ") `NAp` ("++ go a2 ++ ")"
            Right x -> showShort x
            Left err -> show a ++ " not found"

data Instruction = Pushglobal Text
                 | Pushint Int
                 | Push Int
                 | Alloc Int
                 | Update Int
                 | Pop Int
                 | Slide Int
                 | Mkap
                 | Eval
                 | Cond [Instruction] [Instruction]
                 | DyArith DyPrim
                 | Neg
                 | Unwind
                 deriving (Eq, Show)

newtype DyPrim = DyPrim { getDyPrim :: (String, Node -> Node -> ThrowsError Node) }

instance Show DyPrim where
    show = fst . getDyPrim
instance Eq DyPrim where
    (DyPrim (n1, _)) == (DyPrim (n2, _)) = n1 == n2

numericPrims :: [DyPrim]
numericPrims =
 -- try to convert Nodes to Ints
 map (\(n, op) -> DyPrim (n, \xNode yNode -> do
        unless (isNum xNode) $
            throwError $ "operand " ++ show xNode ++ " should be an integer"
        unless (isNum yNode) $
            throwError $ "operand " ++ show yNode ++ " should be an integer"
        let (NNum x) = xNode
            (NNum y) = yNode
        return $ x `op` y
 )) $
 -- arithmetic primitives
    map (\(n, op) -> (n, \x y -> NNum $ x `op` y))
    arithOpFuncs

 -- comparison primitives
 ++ map (\(n, op) -> (n, \x y -> boolToCore $ x `op` y))
    relOpFuncs

applyDyPrim (DyPrim (_, op)) a b = a `op` b

boolToCore True = NNum 1
boolToCore False = NNum 0
coreToBool (NNum 0) = return False
coreToBool (NNum _) = return True
coreToBool x = throwError $ "expected number to use as boolean, found: " ++ show x

data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Text Int [Instruction]
          | NInd Addr
          deriving (Eq, Show)

showShort (NNum i) = "NNum " ++ show i
showShort (NInd i) = "NInd " ++ show i
showShort (NAp a b) = "NAp " ++ show a ++ " " ++ show b
showShort (NGlobal n i _) = "NGlobal " ++ show i ++ " " ++ show n

isIndirection (NInd _) = True
isIndirection _ = False

isNum (NNum _) = True
isNum _ = False

isGlobal (NGlobal{}) = True
isGlobal _ = False

evalMk5 :: GMStateMk5 -> ThrowsError [GMStateMk5]
evalMk5 state = do
    restStates <- if isFinal state then return []
                  else liftM doAdmin (step state) >>= evalMk5
    return $ state : restStates

doAdmin st@(GMS { _statistics = (steps, hstats), _heap = h }) =
    st { _statistics = (steps + 1, _hstats h) }

isFinal = (==[]) . _code

step :: GMStateMk5 -> ThrowsError GMStateMk5
step state = dispatch i (state{ _code = is, _oldcode = i:_oldcode state})
    where (i:is) = _code state




dispatch :: Instruction -> GMStateMk5 -> ThrowsError GMStateMk5

dispatch (Pushglobal f) state = do
    a <- maybeToEither ("undeclared global: " ++ unpack f)
       $ Prelude.lookup f (_globals state)
    return $ state{ _stack = a:_stack state }

dispatch (Pushint n)    state
    | isJust allocd = return $ state{ _stack = fromJust allocd : _stack state }
    | otherwise = return $ state{ _stack = a:_stack state, _heap = heap'
                                , _globals = (pack $ show n,a):_globals state }
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

dispatch (Slide n)      state = do
    let stk = _stack state
    return $ state{ _stack = head stk : drop (n+1) stk }

dispatch (Alloc n)      state =
    let (heap', emptyAddrs) =
            mapAccuml (const . H.alloc (NInd nullAddr)) (_heap state) [1..n]
    in return $ state{ _heap = heap', _stack = emptyAddrs ++ _stack state }

dispatch Mkap           state = do
    let (a1:a2:stk) = _stack state
        (heap', app) = H.alloc (NAp a1 a2) (_heap state)
    return $ state{ _heap = heap', _stack = app:stk }

dispatch Eval           state =
    return $ state{ _code = [Unwind], _stack = [head $ _stack state]
                  , _dump = (_code state, tail $ _stack state) : _dump state }

dispatch (Cond e1 e2)   state = do
    top <- getStackTop state
    bool <- coreToBool top
    return $ state{ _code = (if bool then e1 else e2) ++ _code state
                  , _stack = tail $ _stack state }

dispatch (DyArith f)    state = do
    arg1 <- getStack 0 state
    arg2 <- getStack 1 state
    result <- applyDyPrim f arg1 arg2
    let (heap', a) = H.alloc result (_heap state)
    return $ state{ _heap = heap', _stack = a:drop 2 (_stack state) }

dispatch Neg            state = do
    top <- getStackTop state
    case top of
        NNum n ->
            let (heap', a') = H.alloc (NNum (-n)) (_heap state)
            in return $ state{ _stack = a' : tail (_stack state), _heap = heap' }
        x -> throwError $ "arithmetic function negate expected integer, found: " ++ show x

dispatch Unwind         state = do
    when (null $ _stack state)
        $  throwError "cannot unwind with an empty stack"
    top <- getStackTop state
    let stk = _stack state
        nonEmptyDump = not . null $ _dump state
    -- #1337 hacker
    case () of
     _  | (NNum _) <- top       -- evaluate numbers to WHNF
        , nonEmptyDump ->
            let (code', stack') = head $ _dump state
            in return $ state{ _code = code'
                             , _stack = head stk : stack'
                             , _dump = tail $ _dump state}
        -- following program would fail without this rule
        -- main = (I I 3) * 3
        | (NGlobal _ n _) <- top -- evaluate anything else to WHNF
        , length stk - 1 < n
        , nonEmptyDump ->
            let (code', stack') = head $ _dump state
            in return $ state{ _code = code', _stack = last stk:stack', _dump = tail $ _dump state }
        | (NGlobal name n c) <- top -> do
            newstk <- rearrangeStack name n (_heap state) stk
            return $ state{ _stack = newstk, _code = c }
        | (NAp a1 _) <- top ->
           return $ state{ _code = [Unwind], _stack = a1:stk }
        | (NInd a) <- top ->
            dispatch Unwind $ state{ _stack = a:tail stk }
        | (NNum _) <- top ->
            return state
        | otherwise -> error "impossible"


getStackTop = getStack 0
getStack n state = H.lookup (_stack state !! n) (_heap state)

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

compileMk5 :: CoreProgram -> ThrowsError GMStateMk5
compileMk5 program = do
    (heap, globals) <- do
        compiledProg <- mapM compileSupercombo (prelude ++ program)
        return $ mapAccuml allocateSupercombo H.init (compiledProg ++ compiledPrims)
    return $ GMS initCode [] [] heap globals initStats []
    where   initStats = (0, initHStats)
            initCode = [ Pushglobal "main", Eval ]
            allocateSupercombo heap (GMCompiledSC name nargs ins) =
                (heap', (name, addr))
                where (heap', addr) = H.alloc (NGlobal name nargs ins) heap

compiledPrims =
    [ GMCompiledSC "negate" 1 [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]
    , GMCompiledSC "if" 3 [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind]
    ] ++ map (\f@(DyPrim (name, _)) ->
        GMCompiledSC (pack name) 2 [ Push 1, Eval, Push 1, Eval, DyArith f
                                   , Update 2, Pop 2, Unwind ]) numericPrims



type GMCompiler = [(Text, Int)] -> Expr Text -> ThrowsError [Instruction]

compileSupercombo :: Supercombo Text -> ThrowsError GMCompiledSC
compileSupercombo (Supercombo name env bod) =
    liftM (GMCompiledSC name (length env))
    $   compileR (length env) (zip env [0..]) bod


compileR :: Int -> GMCompiler
compileR arity env e = do
    ins <- compileE env e
    return $ ins ++ [Update arity, Pop arity, Unwind]

-- main = 3 + 4 * 5
-- compileC gives
--       Pushint 5,Pushint 4,Pushglobal "*",Mkap,Mkap
--      ,Pushint 3,Pushglobal "+",Mkap,Mkap
--      ,Update 0,Pop 0,Unwind
-- compileE gives
--       Pushint 5,Pushint 4,DyArith *,Pushint 3,DyArith +
--      ,Update 0,Pop 0,Unwind

compileC :: GMCompiler
compileC _ (Num n) = return [Pushint n]
compileC env (Var v)
    | v `elem` map fst env = return [Push . fromJust $ Prelude.lookup v env]
    | otherwise = return [Pushglobal v]
compileC env (App arg1 arg2) = do
    arg2C <- compileC env arg2
    arg1C <- compileC (argOffset 1 env) arg1
    return $ arg2C ++ arg1C ++ [Mkap]
compileC env (Let recursive defs expr) = compileLet compileC env recursive defs expr
compileC _ _ = throwError "not implemented yet!"


compileE :: GMCompiler
compileE env (Let recursive defs expr) = compileLet compileE env recursive defs expr
compileE env (App (Var "negate") arg) = (++[Neg]) <$> compileE env arg
compileE env (App (App (App (Var "if") cond) branch1) branch2) = do
    ccode <- compileE env cond
    b1code <- compileE env branch1
    b2code <- compileE env branch2
    return $ ccode ++ [Cond b1code b2code]
compileE env x@(App (App (Var binop) arg1) arg2)
    | isJust dyadicPrim = do
        arg2C <- compileE env arg2
        arg1C <- compileE (argOffset 1 env) arg1
        return $ arg2C ++ arg1C ++ [DyArith . fromJust $ dyadicPrim]
    | otherwise = (++[Eval]) <$> compileC env x
  where dyadicPrim = foldl (\a e@(DyPrim (prim, _)) ->
            if prim == unpack binop then Just e else a) Nothing numericPrims
compileE env x  = (++[Eval]) <$> compileC env x

compileLet compileExpr env recursive defs expr
    | recursive = do
        (defsCode, _) <- foldM (\(acc, nth) (_, def) -> do
                code <- compileC env' def
                return ((code ++ [Update nth]) : acc, nth - 1)
            ) ([], len - 1) defs
        exprCode <- compileExpr env' expr
        return $ [Alloc len] ++ (concat . reverse) defsCode
                 ++ exprCode ++ [Slide len]
    | otherwise = do
        (defsCode, _) <- foldM (\(acc, offsetEnv) (_, def) -> do
                code <- compileC offsetEnv def
                return (code : acc, argOffset 1 offsetEnv)
            ) ([], env) defs
        exprCode <- compileExpr env' expr
        return $ (concat . reverse) defsCode ++ exprCode ++ [Slide len]
  where len = length defs
        env' = zip (map fst defs) [len-1, len-2 .. 0] ++ argOffset len env

argOffset n env = do
    (v, m) <- env
    return (v, n + m)
