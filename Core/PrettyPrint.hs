{-# LANGUAGE OverloadedStrings #-}
module Core.PrettyPrint (pprint, pprintExpr) where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)

import Core.Types

(+.+) = T.append

joinSemiSep first sep = T.append (T.pack first) . T.concat . intersperse (T.pack sep)

tabs n l = l ++ replicate n '\t'

-- O(n^2) haha
pprint :: CoreProgram -> Text
pprint =
    joinSemiSep "" ";\n\n" . map (\(Supercombo name bindings bod) ->
            name
        +.+ (if null bindings then T.empty else " ")
        +.+ T.unwords bindings
        +.+ " = "
        +.+ pprintExpr 1 bod)

addParens expr str = if isAtomic expr then str else "(" +.+ str +.+ T.pack ")"

pprintExpr :: Int -> Expr Text -> Text
pprintExpr _ (Var a) = a
pprintExpr _ (Num a) = T.pack $ show a
pprintExpr _ (Constructor t a) =
                T.pack $ "Pack{" ++ show t ++ "," ++ show a ++ "}"
pprintExpr lvl (App g y) =
  case g of
    (App (Var binop) x) ->
            if binop `elem` binops
            then addParens g $
                    pprintExpr lvl x
                +.+ " "
                +.+ binop
                +.+ " "
                +.+ pprintExpr lvl y
            else nonbin g
    _ -> nonbin g
  where nonbin f =
                addParens f (pprintExpr lvl f)
            +.+ " "
            +.+ addParens y (pprintExpr lvl y)
pprintExpr lvl (Let isrec bs bod) = T.pack ("let" ++ if isrec then "rec " else " ")
                            +.+ (if length bs /= 1
                                then joinSemiSep ("\n" ++ tabs lvl "") (tabs lvl ";\n")
                                else T.concat)
                                (map (\(name, expr) ->
                                    name +.+ " = " +.+ pprintExpr (lvl+1) expr
                                ) bs)
                            +.+ T.pack ("\n" ++ tabs lvl "" ++ "in ")
                            +.+ pprintExpr (lvl+1) bod
pprintExpr lvl (Case expr alts) = "case "
                            +.+ addParens expr (pprintExpr lvl expr)
                            +.+ " of "
                            +.+ joinSemiSep ("\n" ++ tabs lvl "") (tabs lvl ";\n")
                                (map (\(tag, bs, subexpr) ->
                                    "<"
                                +.+ T.pack (show tag)
                                +.+ ">"
                                +.+ (if null bs then T.empty else " " +.+ T.unwords bs)
                                +.+ " -> "
                                +.+ pprintExpr (lvl+1) subexpr
                                ) alts)
pprintExpr lvl (Lambda bs bod)    = '\\'
                        `T.cons`T.unwords bs
                            +.+ " . "
                            +.+ pprintExpr lvl bod
