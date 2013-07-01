{-# OPTIONS_GHC -Wall #-}

-- | Syntax utilities

module Koshucode.Baala.Base.Section.Utility
( -- * Term
  termNames
, termNamePairs
, termTreePairs

  -- * Lines
, clausify
, tokenSourceLines
, operandGroup

  -- * Calculation
, Calc
, Ripen
, crop
) where

import qualified Data.Maybe as Maybe

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax



-- ----------------------  Term

-- | Extract a term name.
termName :: TokenTree -> AbortOr String
termName (TreeL (TermN [n])) = Right n
termName x = Left $ AbortMissingTermName [] (show x)

-- | Extract a list of term names.
-- 
--   >>> termNames $ tokenTrees $ tokens "/a /b /c"
--   Right ["/a","/b","/c"]

termNames :: [TokenTree] -> AbortOr [String]
termNames = mapM termName

-- e1 = termNames . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a /b /c"
-- e4 = e1 "/a /b \n /c"
-- e5 = e1 "/a bb /c"

-- | Extract a list of name-and-name pairs.
-- 
--   >>> termNamePairs $ tokenTrees $ tokens "/a /x /b /y"
--   Right [("/a","/x"), ("/b","/y")]

termNamePairs :: [TokenTree] -> AbortOr [(String, String)]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop (a : _) = Left $ AbortMissingTermName [] (show a)

-- e1 = termPairs . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a /x"
-- e4 = e1 "/a /x /b /y"
-- e5 = e1 "/a /x /b /y /c"
-- e6 = e1 "/a /x (/b /y)"

-- | Extract a list of name-and-tree pairs.
-- 
--   >>> termTreePairs $ tokenTrees $ tokens "/a 'A3' /b 10"
--   Right [("/a", TreeL (Word 1 "A3")), ("/b", TreeL (Word 0 "10"))]

termTreePairs :: [TokenTree] -> AbortOr [(String, TokenTree)]
termTreePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           xs' <- loop xs
           Right $ (a', b) : xs'
    loop [] = Right []
    loop (a : _) = Left $ AbortMissingTermName [] (show a)

-- e1 = termTreePairs . tokenTrees . tokens
-- e2 = e1 ""
-- e3 = e1 "/a /x"
-- e4 = e1 "/a (/x + 1) /b /y"
-- e5 = e1 "/a (/x + 1) /b"



-- ---------------------- Clausify

{-| Convert token list into list of token clauses -}
clausify :: [SourceLine] -> [[Token]]
clausify = gather clausifySplit1 where

clausifySplit1 :: [SourceLine] -> ([Token], [SourceLine])
clausifySplit1 = loop where
    loop (SourceLine _ _ [Comment _] : ls)
        = loop ls
    loop (src@(SourceLine _ _ (Space i : xs)) : ls)
        = Line src `cons1` (xs `append1` clausifySplit2 i ls)
    loop (src@(SourceLine _ _ xs@(Word _ _ : _)) : ls)
        = Line src `cons1` (xs `append1` clausifySplit2 0 ls)
    loop (_ : ls) = ([], ls)
    loop [] = ([], [])

clausifySplit2 :: Int -> [SourceLine] -> ([Token], [SourceLine])
clausifySplit2 i = loop where
    loop (src@(SourceLine _ _ (Space n : xs)) : ls)
        | n > i  = Line src `cons1` (xs `append1` loop ls)
    loop ls      = ([], ls)

append1 :: [a] -> ([a], b) -> ([a], b)
append1 x1 (x2, y) = (x1 ++ x2, y)

tokenSourceLines :: [Token] -> [SourceLine]
tokenSourceLines xs = Maybe.mapMaybe tokenSourceLine xs

tokenSourceLine :: Token -> Maybe SourceLine
tokenSourceLine (Line src) = Just src
tokenSourceLine _ = Nothing

-- e1 = mapM_ print . clausify . sourceLines
-- e2 = e1 "a\nb\nc\n\n"
-- e3 = e1 "a\n b\nc\n"
-- e4 = e1 " a\n b\nc\n"
-- e5 = e1 " a\n  b\nc\n"
-- e6 = e1 " a\nb\nc\n"
-- e7 = e1 "\na\nb\n"
-- e8 = e1 "a\n\n b\nc\n"
-- e9 = e1 "a\n  \n b\nc\n"

-- | Split operand into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
-- 
--   >>> operandGroup $ tokenTrees $ tokens "a b -x c 'd' -y e"
--   [("",   [TreeL (Word 0 "a"), TreeL (Word 0 "b")]),
--    ("-x", [TreeL (Word 0 "c"), TreeL (Word 1 "d")]),
--    ("-y", [TreeL (Word 0 "e")])]

operandGroup :: [TokenTree] -> [Named [TokenTree]]
operandGroup = nil . (gather $ anon []) where
    -- add empty operand
    nil xs = case lookup "" xs of
               Nothing -> ("", []) : xs
               Just _  -> xs

    -- anonymous group
    anon ys xs@(TreeL (Word 0 n@('-' : _)) : xs2)
        | ys == []     = named n [] xs2  -- no anonymous group
        | otherwise    = group "" ys xs
    anon ys []         = group "" ys []
    anon ys (x:xs)     = anon (x:ys) xs

    -- named group
    named n ys xs@(TreeL (Word 0 ('-' : _)) : _) = group n ys xs
    named n ys []      = group n ys []
    named n ys (x:xs)  = named n (x:ys) xs

    -- operand group named 'n'
    group n ys xs = ((n, reverse ys), xs)



-- ----------------------  Calculation

-- | Calculator combining values into a single value
type Calc v = [v] -> v

-- | Set of operators
type Ripen x v = x -> [v] -> Calc v

--type TreeMap x y = Tree x -> Tree y

-- | Calculate a value of tree
crop
  :: Ripen x v   -- ^ Set of Operators
  -> Tree x      -- ^ Cropping target
  -> Calc v      -- ^ Calculator
crop ripen tree1 arg = c tree1 [] where
    c (TreeL x)        vs = ripen x vs arg
    c (TreeB _ (x:xs)) vs = c x $ map (`c` vs) xs
    c _ _ = undefined

-- gather a crop
-- harvest a crop
