{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Base.Token.TokenClause
( TokenClause,
  clausify,
) where

import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax          as B
import qualified Koshucode.Baala.Base.Token.Token     as B
import qualified Koshucode.Baala.Base.Token.Tokenize  as B


-- ----------------------  Datatype

type TokenClause = B.CodeClause B.Token

instance B.Pretty TokenClause where
    doc = docTokenClause

docTokenClause :: TokenClause -> B.Doc
docTokenClause (B.CodeClause toks ls) = d where
    d      = B.docHang k 2 $ B.docv [dls, dtoks]
    k      = B.doc "TokenClause"
    dtoks  = labeled toks "[Token]"
    dls    = labeled ls   "[TokenLine]"
    labeled xs name = B.docHang (label xs name) 2 $ B.docv xs
    label   xs name = B.doch [ B.doc name
                             , B.doc $ length xs, B.doc "elements" ]


-- ----------------------  Function

{-| Convert token lines into list of token clauses -}
clausify :: [B.TokenLine] -> [TokenClause]
clausify = B.gather split

split :: [B.TokenLine] -> (TokenClause, [B.TokenLine])
split = loop where
    loop (B.CodeLine _ _ xs : ls) | white xs = loop ls
    loop (src@(B.CodeLine _ _ (B.TSpace _ i : xs)) : ls)
        = cons xs src $ splitWith i ls
    loop (src@(B.CodeLine _ _ xs@(B.TWord _ _ _ : _)) : ls)
        = cons xs src $ splitWith 0 ls
    loop (_ : ls) = (empty, ls)
    loop []       = (empty, [])

splitWith :: Int -> [B.TokenLine] -> (TokenClause, [B.TokenLine])
splitWith i = loop where
    loop ((B.CodeLine _ _ toks) : ls)
        | white toks = loop ls
    loop (src@(B.CodeLine _ _ (B.TSpace _ n : toks)) : ls)
        | n > i      = cons toks src $ loop ls
    loop ls          = (empty, ls)

empty :: TokenClause
empty = B.CodeClause [] []

-- test white line
white :: [B.Token] -> Bool
white = null . B.sweepToken

cons :: [B.Token] -> B.TokenLine -> B.Map (TokenClause, [B.TokenLine])
cons a1 b1 (B.CodeClause a2 b2, c)
    = (B.CodeClause (a1 ++ a2) (b1 : b2), c)

