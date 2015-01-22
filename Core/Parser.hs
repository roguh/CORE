module Core.Parser (parse, parseConstructor, parseFile) where

import Control.Monad
import Control.Monad.Error

import Text.Parsec hiding (parse)
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Text

import Data.Text (Text, pack, unpack)
import Data.Text.IO as Text (readFile)

import Core.Types

parseFile :: FilePath -> IO (ThrowsError [Supercombo Text])
parseFile fname = Text.readFile fname >>= return . Core.Parser.parse fname

parseConstructor txt = show `left` runParser constructor () "" txt

parse :: FilePath -> Text -> ThrowsError [Supercombo Text]
parse fname txt = show `left` runParser (sepBy1 supercombo (char ';')) () fname txt

f `left` (Left l) = throwError $ f l
_ `left` (Right r) = return r

supercombo :: Parser (Supercombo Text)
supercombo = do
    name <- variable
    bindings <- many (try variable) <?> "zero or more names"
    ignoredText
    char '='
    body <- expr
    ignoredText
    return $ Supercombo name bindings body

ignoredText = many $ try ((void space) <|> comment <|> lineComment)

comment = do string "{-"
             void $ manyTill anyChar (try (string "-}"))

lineComment = do string "||"
                 void $ manyTill anyChar (try (char '\n'))

expr        :: Parser (Expr Text)
expr = ignoredText >> (try letExpr <|> try caseExpr <|> lambda <|> expr1)

assembleOp left Nothing            = left
assembleOp left (Just (op, right)) =
    App (App (Var $ pack op) left) right

-- expr[1 .. 5] define operator precedence and associativity
tryOpOrExpr left opsRights = do
    l <- left
    maybeRight <- choice . (++[return Nothing]) $ map (\(op, right) -> try $ do
        ignoredText
        op' <- op
        ignoredText
        right' <- right
        return $ Just (op', right')) opsRights
    return $ assembleOp l maybeRight

expr1 = tryOpOrExpr expr2 [(string "|", expr1)]
expr2 = tryOpOrExpr expr3 [(string "&", expr2)]
expr3 = tryOpOrExpr expr4 [(choice $ map (try . string) relops, expr4)]
expr4 = tryOpOrExpr expr5 [(string "+", expr4), (string "-", expr5)]
expr5 = tryOpOrExpr expr6 [(string "*", expr5), (string "/", expr6)]
expr6 = many1 (try aexpr) >>= mkAppChain
  where mkAppChain = return . foldl1 App

aexpr :: Parser (Expr Text)
aexpr = ignoredText >>
     (  try (liftM Num number)
    <|> try constructor
    <|> try (liftM Var variable)
    <|> between (ignoredText >> char '(') (ignoredText >> char ')') expr)

variable    :: Parser Text
variable = (do
    ignoredText
    a <- letter
    rest <- many (alphaNum <|> char '_')
    let str = (a:rest)
    mapM_ (\r -> when (str == r) (unexpected $ "reserved " ++ r)) reserved
    return $ pack str) <?> "name"

number      :: Parser Int
number = liftM read (many1 digit)

constructor :: Parser (Expr Text)
constructor = do
    string "Pack{"
    tag <- number
    ignoredText
    char ','
    arity <- number
    ignoredText
    char '}'
    return $ Constructor tag arity

letExpr     :: Parser (Expr Text)
letExpr = do
    string "let"
    isRecursive <- liftM (/=Nothing) $ optionMaybe (string "rec")
    ignoredText
    defns <- sepBy1 (try defn) (try $ char ';')
    ignoredText
    string "in"
    e <- expr
    return $ Let isRecursive defns e
  where defn = do
            name <- variable
            ignoredText
            char '='
            body <- expr
            ignoredText
            return (name, body)

sepByTry1 p sep = do
    x <- p
    xs <- many (try (sep >> p))
    return (x:xs)

caseExpr    :: Parser (Expr Text)
caseExpr = do
    string "case"
    e <- expr
    ignoredText
    string "of"
    alts <- sepByTry1 (try alt) (try $ char ';')
    return $ Case e alts
  where alt = do
            ignoredText
            tag <- (between (char '<' >> ignoredText) (ignoredText >> char '>') number) <?> "tag of form <number>"
            ignoredText
            bindings <- (sepEndBy (try variable) ignoredText) <?> "zero or more names"
            string "->"
            e <- expr
            ignoredText
            return (tag, bindings, e)

lambda      :: Parser (Expr Text)
lambda = do
    char '\\'
    args <- sepByTry1 variable ignoredText <?> "one or more names"
    ignoredText
    char '.'
    e <- expr
    return $ Lambda args e
