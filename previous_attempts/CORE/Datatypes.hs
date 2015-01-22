module Datatypes 
where

type Name = String

type CoreExpr = Expr Name

 -- x + y
 -- is 
 -- EAp (EAp (EVar "+") (EVar "x")) (EVar "y")

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = not recursive

 -- Each definition is a pair of the
 -- var name and the corresponding expression

bindersOf :: [(a,b)] -> [a]
bindersOf defns = do
    (name, rhs) <- defns
    return name

 -- (right hand sides of)

rhssOf :: [(a,b)] -> [b]
rhssOf defns = do
    (name, rhs) <- defns
    return rhs

 -- case expressions need to know their alternatives
 -- which hold tags, bound var.s and the expression

type Alt a = (Integer, [a], Expr a)

type CoreAlt = Alt Name
type CoreAlts = [Alt Name]


isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

--
--
----dun duhn dun dun!!!
--
--

type Defn a = (a, Expr a)

type CoreDefn = Defn Name

data Expr a
 = EVar a
 | ENum Integer 
 | EConstr Integer Integer  --constructor: tag arity
 | EAp (Expr a) (Expr a)    --apply
 | ELet 
    IsRec           --recursive?
    [Defn a]   --definitions
    (Expr a)        --body of
 | ECase
    (Expr a)    --expression to scrutinize
    [Alt a]   --alternatives
 | ELam [a] (Expr a)
 deriving (Show, Eq)


data PartialExpr = NoOp | FoundOp Name CoreExpr
    deriving (Show, Eq)



type Program a = [ScDefn a]
type ScDefn a  = (Name, [a], Expr a)

type CoreProgram = [CoreScDefn]
type CoreScDefn  = ScDefn Name

 -- consider:
 -- main = double 21 ;
 -- double x = x + x
 --
 -- this is represented in Haskell as:
 --
 -- [ ("main", [], (EAp (EVar "double") (ENum 21)))
 -- , ("double", ["x"], (EAp (EAp (EVar "+") ("EVar "x")) (EVar "x"))
 -- ] :: CoreProgram

{--
Core syntax:
    A table of infix ops and info follows:
    precedence, associativity, op
    
    6, left,  application
    5, right, *
     , nope , /
    4, right, +
     , nope,  -
    3, nooo,  == ~= > >= < <=
    2, right, &
    1, right, |
    
    "negate" and "not" are not binary and
    thus provided as functions.
    
    no associativity means
    1/2/3 is illegal

bnf grammar:

PROGRAMS
    program -> sc1; .. ;scn
        n >= 1
    
SUPERCOMBINATORS
    sc -> var var1 .. varn = expr
        n >= 0
    
EXPRESSIONS
    expr -> expr aexpr
        | expr1 binop expr2
        | let defns in expr
        | letrec defns in expr
        | case expr of alts
        | \ var1 .. varn . expr 
            n >= 1
        | aexpr 
    
    Atomic
    aexpr -> var | num | Pack{num,num} | ( expr )
    
DEFINITIONS
    defns -> defn1; .. defn
        n >= 1
    defn = var = expr
    
ALTERNATIVES
    alts -> alt1; .. ;altn
    alt -> <num> var1 .. varn -> expr 
--}
