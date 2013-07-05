{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Section.Clausify
( ClauseSource (..)
, clausify
, operandGroup
) where

import Data.Generics

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax




-- ---------------------- 

data ClauseSource = ClauseSource
    { clauseTokens :: [Token]       -- ^ Source tokens of clause
    , clauseLines  :: [SourceLine]  -- ^ Source lines of clause
    } deriving (Show, Data, Typeable)

emptyClauseSource :: ClauseSource
emptyClauseSource = ClauseSource [] []

{-| Convert token list into list of token clauses -}
clausify :: [SourceLine] -> [ClauseSource]
clausify = gather clausifySplit

clausifySplit :: [SourceLine] -> (ClauseSource, [SourceLine])
clausifySplit = loop where
    loop (SourceLine _ _ xs : ls)
        | white xs = loop ls
    loop (src@(SourceLine _ _ (TSpace _ i : xs)) : ls)
        = cons xs src $ clausifySplitWith i ls
    loop (src@(SourceLine _ _ xs@(TWord _ _ _ : _)) : ls)
        = cons xs src $ clausifySplitWith 0 ls
    loop (_ : ls) = (emptyClauseSource, ls)
    loop []       = (emptyClauseSource, [])

clausifySplitWith :: Int -> [SourceLine] -> (ClauseSource, [SourceLine])
clausifySplitWith i = loop where
    loop ((SourceLine _ _ xs) : ls)
        | white xs = loop ls
    loop (src@(SourceLine _ _ (TSpace _ n : xs)) : ls)
        | n > i    = cons xs src $ loop ls
    loop ls        = (emptyClauseSource, ls)

white :: [Token] -> Bool
white xs = (sweepToken xs == [])

cons :: [Token] -> SourceLine -> Map (ClauseSource, [SourceLine])
cons a1 b1 (ClauseSource a2 b2, c)
    = (ClauseSource (a1 ++ a2) (b1 : b2), c)

-- e1 = mapM_ print . clausify . sourceLines
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
  
    >>> operandGroup $ tokenTrees $ tokens "a b -x c 'd' -y e"
    [("",   [TreeL (Word 0 "a"), TreeL (Word 0 "b")]),
     ("-x", [TreeL (Word 0 "c"), TreeL (Word 1 "d")]),
     ("-y", [TreeL (Word 0 "e")])]
  -}

operandGroup :: [TokenTree] -> [Named [TokenTree]]
operandGroup = nil . (gather $ anon []) where
    -- add empty operand
    nil xs = case lookup "" xs of
               Nothing -> ("", []) : xs
               Just _  -> xs

    -- anonymous group
    anon ys xs@(TreeL (TWord _ 0 n@('-' : _)) : xs2)
        | ys == []     = named n [] xs2  -- no anonymous group
        | otherwise    = group "" ys xs
    anon ys []         = group "" ys []
    anon ys (x:xs)     = anon (x:ys) xs

    -- named group
    named n ys xs@(TreeL (TWord _ 0 ('-' : _)) : _) = group n ys xs
    named n ys []      = group n ys []
    named n ys (x:xs)  = named n (x:ys) xs

    -- operand group named 'n'
    group n ys xs = ((n, reverse ys), xs)

