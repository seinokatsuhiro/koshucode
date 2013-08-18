{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.Clausify
( ClauseSource (..),
  clausify,
  sortOperand,
) where

import Data.Generics

import qualified Koshucode.Baala.Base as B



-- ---------------------- 

data ClauseSource = ClauseSource
    { clauseTokens :: [B.Token]     -- ^ Source tokens of clause
    , clauseLines  :: [B.TokenLine] -- ^ Source lines of clause
    } deriving (Show, Data, Typeable)

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

{-| Split operand into named group.
    Non quoted words beginning with hyphen, e.g., @-x@,
    are name of group.
  
    >>> sortOperand $ B.tokenTrees $ B.tokens "a b -x /c 'd -y e"
    [ ("",   [TreeL (TWord 1 0 "a"),TreeL (TWord 3 0 "b")])
    , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TWord 9 0 "'"), TreeL (TWord 10 0 "d")])
    , ("-y", [TreeL (TWord 14 0 "e")])]
  -}

sortOperand :: [B.TokenTree] -> [B.Named [B.TokenTree]]
sortOperand = nil . (B.gather $ anon []) where
    -- add empty operand
    nil xs = case lookup "" xs of
               Nothing -> ("", []) : xs
               Just _  -> xs

    -- anonymous group
    anon ys xs@(B.TreeL (B.TWord _ 0 n@('-' : _)) : xs2)
        | ys == []     = named n [] xs2  -- no anonymous group
        | otherwise    = group "" ys xs
    anon ys []         = group "" ys []
    anon ys (x:xs)     = anon (x:ys) xs

    -- named group
    named n ys xs@(B.TreeL (B.TWord _ 0 ('-' : _)) : _) = group n ys xs
    named n ys []      = group n ys []
    named n ys (x:xs)  = named n (x:ys) xs

    -- operand group named 'n'
    group n ys xs = ((n, reverse ys), xs)

