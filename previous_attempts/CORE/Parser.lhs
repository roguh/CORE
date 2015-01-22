Here, a parser for the Core language will be defined.
Though parsers are terribly tedious to program
, simple ones are easy to make in functional languages.

> module Parser
> ( parse
> , clex
> , syntax
> , Token
> , parseDebug
> ) where


> import Data.Char (isAlpha, isNumber, isSpace, isAlphaNum)
> import Control.Monad (liftM)

> import Datatypes

This is the lexical analysis function:

> clex :: String -> LineNumber -> [Token]

And this is the syntax analyzer:

> syntax :: [Token] -> CoreProgram

> parse :: String -> CoreProgram
> parse s = syntax (clex s 1)

> parseDebug s = syntaxDebug (clex s 1)

Tokens are strings + line numbers. 

> type LineNumber = Int
> type Token = (LineNumber, String)


> tokenLine :: Token -> LineNumber
> tokenLine (l, _) = l

> upLine :: Token -> Token
> upLine (l, s) = (l + 1, s)

> getToken :: Token -> String
> getToken (_, s) = s

> clex cc@(c:cs) l

Skip whitespace, keep track of lines

>   | isSpace c = clex cs (if c == '\n' then l + 1 else l)

and comments that start with || and go on till '\n'

>   | c == '|' && (head cs == '|') =
>       let (_, noncomment) = span (/= '\n') cs
>       in clex (tail noncomment) (l + 1)

These are special twochar operators, bit o read ahead.

>   | isTwoLoveLove c (head cs) = (l, [c, head cs]) : clex (tail cs) l

Chomp digits & IDs

>   | isNumber c = let (num_token, rest_cs) = span isNumber cc
>       in (l, num_token) : clex rest_cs l

>   | isAlpha c = let (var_tok, rest_cs) = span isIdChar cc
>       in (l, var_tok) : clex rest_cs l

If all else fails...

> clex (c:cs) l = (l, [c]) : clex cs l
> clex [] _ = []


Helpers.

> isIdChar :: Char -> Bool 
> isIdChar c = isAlpha c || isNumber c || (c == '_')

> isTwoLoveLove :: Char -> Char -> Bool
> isTwoLoveLove a b = [a, b] `elem` twoCharOps

> twoCharOps :: [String]
> twoCharOps = ["==", ">=", "<=", "~=", "->"]










As a running example for parsing, here is a grammar:

gretting -> hg person !
hg -> hello | goodbye

person = any token beginning with a letter.




The parser will support indeterminate parses through lists.

> newtype Parser a = Parser { runParse :: [Token] -> (Either String a, [Token]) }

Parsers can fail and be composed together.

> instance Monad Parser where
>   return s = Parser $ \tks -> (Right s, tks)

>   p >>= mkP = Parser $ \tks ->
>       case runParse p tks of
>           (Right rr, tks2) -> runParse (mkP rr) tks2
>           (Left err, tks2) -> (Left err, tks2)

>   fail s = Parser $ \tks -> 
>       case tks of 
>       [] ->     (Left $ s ++ ". near final token", [])
>       (tk:_) -> (Left $ s ++ ". near line #" ++ show (tokenLine tk), tks)

> s <!> err = Parser $ \tks ->
>    case runParse s tks of
>        (Right e, tks2) -> (Right e, tks2)
>        (Left errmsg1, tks2) -> flip (,) tks2 
>           $ Left $ err ++ "\n" ++ errmsg1 ++ " " ++ (
>           case tks2 of 
>               [] -> ". near final token"
>               (tk:_) -> ". near line #" ++ show (tokenLine tk))


> instance Functor Parser where
>   fmap = liftM  

> readNext :: Parser String
> readNext = Parser $ \tkns ->
>   case tkns of
>       [] -> (Left "out of input.", [])
>       (tk:tks) -> (Right $ getToken tk, tks)

> parseGo :: [Token] -> Parser a -> Either String a
> parseGo tks p = 
>   case runParse p tks of
>       (Right res, _) -> Right res
>       (Left err, _)  -> Left err


> parseSeeRem :: [Token] -> Parser a -> (Either String a, [Token])
> parseSeeRem tks p = runParse p tks




This one looks for a specific string and only parses
if it is found.
If not, it gives an empty list

> pLit :: String -> Parser String
> pLit s = 
>    pSat (==s) $ "token not found: '" ++ s ++ "'"




> pAlt :: Parser a -> Parser a -> Parser a
> pAlt p1 p2 = Parser $ \tks ->
>   case runParse p1 tks of
>       (Right r, tks2) -> (Right r, tks2)
>       (Left _, _) -> runParse p2 tks



> pHelloOrGoodBye :: Parser String
> pHelloOrGoodBye = pLit "hello" `pAlt` pLit "goodbye"

> pGreeting :: Parser (String, String)
> pGreeting = do
>   a <- pHelloOrGoodBye
>   b <- pVar
>   c <- pLit "!"
>   return (a, b)



Absolute nice.

> pZeroOrMore :: Parser a -> Parser [a]
> pZeroOrMore p = pOneOrMore p `pAlt` return []

> pOneOrMore :: Parser a -> Parser [a]
> pOneOrMore p = 
>   p >>= \r ->
>   pZeroOrMore p >>= \rs ->
>   return (r:rs)

> pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
> pOneOrMoreWithSep p sep = do
>   rs <- pZeroOrMore 
>       (p >>= \res -> sep >> return res)
>   r <- p
>   return (r:rs)



> pSat :: (String -> Bool) -> String -> Parser String
> pSat f err = do
>   n <- readNext
>   if f n
>       then return n
>       else fail err

> pVar :: Parser String
> pVar = pSat isVar "variable not found"
>   where isVar v@(c:s) =

Variables aren't keywords
, start with alphas and 
the rest of the chars are alphanumerics or _

>           v `notElem` keywords &&
>           isAlpha c && 
>           all (==True) (map (\c -> isAlphaNum c || c == '_') s)

> keywords :: [String]
> keywords = ["let","letrec","case","in","of","pack"] 



> pNum :: Parser Integer
> pNum = pSat (all (==True) . map isNumber) "expected number" >>= \n ->
>   return $ read n




Now back to the Core language.

> syntax s = case parseGo s pProgram of
>   Right r -> r
>   Left e  -> error e 

> syntaxDebug :: [Token] -> (Either String CoreProgram, [Token])
> syntaxDebug s = parseSeeRem s pProgram


We can define this code just by transliterating the original BNF grammar!

> pProgram :: Parser CoreProgram
> pProgram = pOneOrMoreWithSep pSc (pLit ";")

> pSc :: Parser CoreScDefn
> pSc = do
>   a <- pVar
>   bs <- pZeroOrMore pVar
>   pLit "="
>   ex <- pExpr
>   return $ mkSc a bs ex

> mkSc :: Name -> [Name] -> Expr Name -> ScDefn Name
> mkSc a bs ex = (a, bs, ex) 




> pExpr :: Parser CoreExpr
> pExpr = 
>   (pAexpr `pAlt`
>   pCLet `pAlt`
>   pCLetRec `pAlt`
>   pCLam  `pAlt`
>   pCCase
>   -- `pAlt`
>   --pCApp `pAlt`
>   --pCBinOp -- binop last, needs 2 expr
>   ) <!> "no expression found"

> pAexpr :: Parser CoreExpr
> pAexpr = (pCVar `pAlt` pCNum `pAlt` pCPack `pAlt` pCParens) <!> "no atomic expression found"

> pCLet' :: Bool -> Parser CoreExpr
> pCLet' rec = do 
>   pLit $ "let" ++ (if rec then "rec" else "")
>   ds <- pDefns
>   pLit "in"
>   e <- pExpr
>   return $ ELet rec ds e


> pCLet :: Parser CoreExpr
> pCLet = pCLet' False

> pCLetRec :: Parser CoreExpr
> pCLetRec = pCLet' True 

> pCCase :: Parser CoreExpr
> pCCase = do
>   pLit "case"
>   e <- pExpr
>   pLit "of"
>   as <- pCAlts 
>   return $ ECase e as

> pCLam :: Parser CoreExpr
> pCLam = do
>   pLit "\\"
>   vs <- pOneOrMore pVar
>   pLit "."
>   e <- pExpr
>   return $ ELam vs e

> pCApp :: Parser CoreExpr
> pCApp = fail "I CANNOT READ THIS NONSENSE!"

EAp expr expr

> pCBinOp :: Parser CoreExpr
> pCBinOp = pCApp 

EAp e1 e2






> pCVar :: Parser CoreExpr
> pCVar = fmap EVar pVar 
 
> pCNum :: Parser CoreExpr
> pCNum = fmap ENum pNum
 
> pCPack :: Parser CoreExpr
> pCPack = do 
>   pLit "Pack"
>   pLit "{"
>   n1 <- pNum
>   pLit ","
>   n2 <- pNum
>   pLit "}"
>   return $ EConstr n1 n2
 
> pCParens :: Parser CoreExpr
> pCParens = do
>   pLit "("
>   e <- pExpr 
>   pLit ")"
>   return e


> pDefns :: Parser [Defn Name]
> pDefns = pOneOrMoreWithSep pDefn (pLit ";")

> pDefn :: Parser (Defn Name)
> pDefn = do
>   v <- pVar
>   pLit "="
>   e <- pExpr
>   return (v, e)

> pCAlts :: Parser CoreAlts
> pCAlts = pOneOrMoreWithSep pCAlt (pLit "/")

> pCAlt :: Parser CoreAlt
> pCAlt = do
>   pLit "<"
>   n <- pNum
>   pLit ">"
>   vs <- pZeroOrMore pVar
>   pLit "->"
>   e <- pExpr
>   return (n, vs, e) 



> pBinOp :: Parser String 
> pBinOp = arithOp `pAlt` relOp `pAlt` boolOp
>   where arithOp = pSat (oneOf ["+", "-", "*", "/"]) "expected arithmetic op"
>         relOp   = pSat (oneOf ["<","<=",">=",">","==","~="]) "expected compare op"
>         boolOp  = pSat (oneOf ["&","|"]) "expected boolean op"

> oneOf :: (Eq a) => [a] -> a -> Bool
> oneOf ss s = True `elem` map (==s) ss








parseGo (clex "let x = y in y" 0) pCLet

parseGo (clex "x = y ; y = x" 0) pDefns

parseGo (clex "x = y" 0) pDefns

parseGo (clex "case (let x = y) of <1> -> 2 ; <2> -> 10" 0) pCase
