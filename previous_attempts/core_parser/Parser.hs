{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser (parseCore, parseCoreFile) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Expr

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)

import Datatypes

parseCoreFile :: FilePath -> IO (Either String [Supercombinator Text])
parseCoreFile fp = Text.readFile fp >>= return . parseCore

parseCore :: Text -> Either String [Supercombinator Text]
parseCore txt = show `left` runParser (many1 supercombinator) () "<input>" txt
    where f `left` (Left l) = Left $ f l
          _ `left` (Right r) = Right r 

supercombinator :: Parsec Text u (Supercombinator Text)
supercombinator = do
    name <- variable
    vars <- many variable
    _ <- string "->"
    spaces
    _ <- char '='
    spaces
    ex <- expr
    spaces
    return (name, vars, ex)

type ParseExpr s u a = Parsec Text u (Expr a)

variable :: Parsec s u a
variable = undefined

number :: Parsec s u CoreNum
number = undefined

expr :: ParseExpr s u a 
expr = aexpr <|> lambdaExpr <|> letExpr <|> caseExpr <|> binopExpr

aexpr :: ParseExpr s u a 
aexpr = (variable >>= return . EVar) <|> (number >>= return . ENum) 
     <|> packExpr <|> between (char '(') (char ')') expr

defn :: Parsec s u (Defn a)
defn = undefined

alt :: Parsec s u (Alt a)
alt = undefined 

letExpr :: ParseExpr s u a
letExpr = undefined 

caseExpr :: ParseExpr s u a 
caseExpr = undefined 

lambdaExpr :: ParseExpr s u a 
lambdaExpr = undefined 

packExpr :: ParseExpr s u a 
packExpr = undefined 

binopExpr :: ParseExpr s u a 
binopExpr = undefined 
