{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.Clausify
( ClauseSource (..),
  clausify,
) where

import qualified Data.Generics as G
import qualified Koshucode.Baala.Base as B

data ClauseSource = ClauseSource
    { clauseTokens :: [B.Token]     -- ^ Source tokens of clause
    , clauseLines  :: [B.TokenLine] -- ^ Source lines of clause
    } deriving (Show, G.Data, G.Typeable)

emptyClauseSource :: ClauseSource
emptyClauseSource = ClauseSource [] []

{-| Convert token list into list of token clauses -}
clausify :: [B.TokenLine] -> [ClauseSource]
clausify = B.gather clausifySplit

clausifySplit :: [B.TokenLine] -> (ClauseSource, [B.TokenLine])
clausifySplit = loop where
    loop (B.CodeLine _ _ xs : ls)
        | white xs = loop ls
    loop (src@(B.CodeLine _ _ (B.TSpace _ i : xs)) : ls)
        = cons xs src $ clausifySplitWith i ls
    loop (src@(B.CodeLine _ _ xs@(B.TWord _ _ _ : _)) : ls)
        = cons xs src $ clausifySplitWith 0 ls
    loop (_ : ls) = (emptyClauseSource, ls)
    loop []       = (emptyClauseSource, [])

clausifySplitWith :: Int -> [B.TokenLine] -> (ClauseSource, [B.TokenLine])
clausifySplitWith i = loop where
    loop ((B.CodeLine _ _ xs) : ls)
        | white xs = loop ls
    loop (src@(B.CodeLine _ _ (B.TSpace _ n : xs)) : ls)
        | n > i    = cons xs src $ loop ls
    loop ls        = (emptyClauseSource, ls)

white :: [B.Token] -> Bool
white xs = (B.sweepToken xs == [])

cons :: [B.Token] -> B.TokenLine -> B.Map (ClauseSource, [B.TokenLine])
cons a1 b1 (ClauseSource a2 b2, c)
    = (ClauseSource (a1 ++ a2) (b1 : b2), c)

-- e1 = mapM_ print . clausify . tokenize
-- e2 = e1 "a\nb\nc\n\n"
-- e3 = e1 "a\n b\nc\n"
-- e4 = e1 " a\n b\nc\n"
-- e5 = e1 " a\n  b\nc\n"
-- e6 = e1 " a\nb\nc\n"
-- e7 = e1 "\na\nb\n"
-- e8 = e1 "a\n\n b\nc\n"
-- e9 = e1 "a\n  \n b\nc\n"
