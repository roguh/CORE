module PrettyPrint
where

import Data.List (intersperse)

import Datatypes

{- DL -> delayed list
 - print program
 - print scs
 - p expr
 - p defs
 - p alts
 - p bins (partially applied too)
 - p vars
 - p ns
 - 
 -}

data DL a = Nil
		  | E a (DL a)
		  | App (DL a) (DL a)
		  deriving (Show)

toList :: DL a -> [a]
toList Nil          = []
toList (E a rest) = a : toList rest
toList (App l1 l2)  = (toList l1) ++ (toList l2)

concatDL :: [DL a] -> DL a
concatDL = foldl1 (flip App)

intersperseDL :: a -> [DL a] -> DL a
intersperseDL ne dl = foldl1 (\acc e -> App (App acc (E ne Nil)) e) dl

pprint :: CoreProgram -> String
pprint = concat . toList . pProgram 

pProgram :: CoreProgram -> DL String
pProgram p = intersperseDL " ;\n" $ map pScDefn p

pScDefn :: ScDefn Name -> DL String
pScDefn (name, vs, e) = 
	E (name ++ (if null vs then "" else " ")) Nil `App`
	pVars vs `App`
	E " = " Nil `App`
	pExpr 1 False inline e

lined, inline :: String
lined = "\n"
inline = ""

pExpr :: Int -> Bool -> String -> CoreExpr -> DL String
pExpr depth parens prepend expr = 
	case expr of
		EVar a -> end $ E a Nil
		
		ENum n -> end $ E (show n) Nil 
		
		EConstr id nargs -> end $ E ("Pack{" ++ show id ++ "," ++ show nargs ++ "}") Nil 
		
		ELet isrec defs e -> end $ 
			E slet Nil `App` 
			pDefs depth inline defs `App` 
			E " in " Nil `App` 
			pExpr depth False inline e 
				where slet = "let" ++ (if isrec then "rec " else " ")
			
		ECase e alts -> end $ 
			E "case " Nil `App` 
			pExpr depth (if isAtomicExpr e then False else True) inline e `App` 
			E " of" Nil `App` 
			pAlts (depth + 1) ("\n" ++ (take depth $ cycle "\t")) alts
		
		ELam vs e -> end $ 
			E "\\" Nil `App` 
			pVars vs `App` 
			E ". " Nil `App` 
			pExpr depth False inline e
			
	where end ex = 
		(if null prepend 
			then E startp Nil 
			else E prepend Nil) 
			
		`App` ex `App` 
		
		(if null prepend 
			then E endp Nil 
			else Nil)
			
		where startp = if parens then "(" else ""; endp = if parens then ")" else ""
			
pVars :: [Name] -> DL String
pVars []     = Nil
pVars (x:xs) = (E x Nil) `App` 
	(if null xs then Nil else (E " " Nil) `App` (pVars xs))

pDefs :: Int -> String -> [CoreDefn] -> DL String
pDefs _ _ [] = Nil
pDefs depth prepend ((name, e) : rest) = 
	E name Nil `App` E " = " Nil `App` pExpr depth False prepend e `App`
	if null rest then Nil
			  else E (";" ++ endc) Nil `App` pDefs depth prepend rest -----------newl or space!?
	where endc = if null prepend then " " else prepend

pAlts :: Int -> String -> [CoreAlt] -> DL String
pAlts _ _ [] = Nil
pAlts depth prepend ((n, vs, e) : rest) = 
	E (prepend ++ "<" ++ show n ++ ">" ++ 
		(if null vs then "" else " ")) Nil `App` -----------tab!
	pVars vs `App` E " -> " Nil `App`
	pExpr depth False inline e `App` 
	if null rest then Nil else E " ; " Nil `App`
	pAlts depth prepend rest 

--app & bin?
--pPartial -> pExpr
