module Core.Types where

import Data.Text (Text, pack)
import Data.Text as T (concat)

type Tag       = Int
type Arity     = Int
type IsRecursive = Bool

-- 'a' is the type of binding labels, usually just String or similar
data Expr a = Var a
            | Num Int
            | Constructor Tag Arity
            | App (Expr a) (Expr a)
            | Let IsRecursive [(a, Expr a)] (Expr a)
            | Case (Expr a) [(Tag, [a], Expr a)]
            | Lambda [a] (Expr a)
        deriving (Show, Eq)

data Supercombo a = Supercombo { _name :: Text, _bindings :: [a], _body :: Expr a }
        deriving (Show, Eq)

type ThrowsError a = Either String a

type CoreProgram = [Supercombo Text]

showErr = ("ERROR: " ++)

isAtomic (Var _) = True
isAtomic (Num _) = True
isAtomic (Constructor _ _) = True
isAtomic _ = False

reserved = ["case", "of", "let", "letrec", "in", "Pack"]

binops = map pack $ map fst boolOpFuncs ++ relops ++ arithops

relops = map fst (relOpFuncs :: [(String, () -> () -> Bool)])

arithops = map fst (arithOpFuncs :: [(String, Int -> Int -> Int)])

arithOpFuncs = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

boolOpFuncs = [("|", (||)), ("&", (&&))]

relOpFuncs :: (Ord a, Eq a) => [(String, a -> a -> Bool)]
relOpFuncs = [ ("<=", (<=)), (">=", (>=)), ("<", (<)), (">", (>)), ("==", (==)), ("~=", (/=)) ]


