{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.TokenClause
( TokenClause (..),
  clausify,
) where

import qualified Data.Generics as G
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax.CodeLine as B
import qualified Koshucode.Baala.Base.Syntax.Token    as B
import qualified Koshucode.Baala.Base.Syntax.Tokenize as B


-- ----------------------  Datatype

data TokenClause = TokenClause
    { tokenList :: [B.Token]       -- ^ Source tokens of clause
    , tokenLines  :: [B.TokenLine] -- ^ Source lines of clause
    } deriving (Show, G.Data, G.Typeable)

instance B.Pretty TokenClause where
    doc = docTokenClause

docTokenClause :: TokenClause -> B.Doc
docTokenClause (TokenClause toks ls) = d where
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
    loop (B.CodeLine _ _ xs : ls)
        | white xs = loop ls
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
        | n > i    = cons toks src $ loop ls
    loop ls        = (empty, ls)

empty :: TokenClause
empty = TokenClause [] []

-- test white line
white :: [B.Token] -> Bool
white xs = (B.sweepToken xs == [])

cons :: [B.Token] -> B.TokenLine -> B.Map (TokenClause, [B.TokenLine])
cons a1 b1 (TokenClause a2 b2, c)
    = (TokenClause (a1 ++ a2) (b1 : b2), c)

