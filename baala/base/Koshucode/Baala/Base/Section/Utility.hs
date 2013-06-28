{-# OPTIONS_GHC -Wall #-}

-- | Syntax utilities

module Koshucode.Baala.Base.Section.Utility
( -- * Term
  termNames
, termNamePairs
, termTreePairs

  -- * Lines
, clausify
, sourceLines
, operandGroup

  -- * Calculation
, Calc
, Ripen
, crop
) where

import Koshucode.Baala.Base.Syntax.TokenTree
import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import qualified Data.Maybe as Maybe



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
clausify :: [Token] -> [[Token]]
clausify = gather clausifySplit

{-| Split into first clause and rest tokens -}
clausifySplit :: [Token] -> ([Token], [Token])
clausifySplit = loop zero where
    zero = Line zeroLine
    loop _  (ln@(Line _) : xs)  = loop ln xs
    loop ln ((Comment _) : xs)  = loop ln xs
    loop ln (Space i : xs) = clausifySplit2 i ln xs -- initial indent is 'i' spaces
    loop ln xs             = clausifySplit2 0 ln xs -- no indent

clausifySplit2 :: Int -> Token -> [Token] -> ([Token], [Token])
clausifySplit2 i ln xs = ln `cons1` mid xs where
    -- middle of line
    mid xxs@(Line _ : _)    = beg xxs   -- next line
    mid (x : xs2)           = x `cons1` mid xs2
    mid xxs                 = ([], xxs)

    -- beginning of line
    beg (x1@(Line _) : x2@(Space n) : xs2)
        | n > i = x1 `cons1` (x2 `cons1` mid xs2) -- indented line
    beg xxs     = ([], xxs)                   -- non indented line

zeroLine :: SourceLine
zeroLine = SourceLine 0 "" []

sourceLines :: [Token] -> [SourceLine]
sourceLines xs = Maybe.mapMaybe sourceLine xs

sourceLine :: Token -> Maybe SourceLine
sourceLine (Line src) = Just src
sourceLine _ = Nothing

-- e1 = mapM_ print . clausify . tokens
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
