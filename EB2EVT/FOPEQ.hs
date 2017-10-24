{-# LANGUAGE OverloadedStrings #-}
--- This is just the interface to FOPEQ
--- Everything's a string, so this is simple.

module FOPEQ where

import Data.List(intercalate)
import Syntax

type Formula = String
type Term = String

parenth exp = "(" ++ exp ++ ")"


lt :: Term -> Term -> Formula
lt e1 e2 = parenth (e1 ++ " < " ++ e2)

le :: Term -> Term -> Formula
le e1 e2 = parenth (e1 ++ " <= " ++ e2)

and :: [Formula] -> Formula
and exps = parenth (intercalate " & " exps)

forall :: [(Ident, Ident)] -> Formula -> Formula
forall ids body = parenth ("FA " ++ concat (anys ids) ++ ". " ++ body)

anys:: [(Ident, Ident)] -> [String]
anys al = Prelude.map (\(x,y) -> (x++":")++(y))al

