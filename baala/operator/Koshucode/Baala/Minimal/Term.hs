{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Minimal.Term
( termNames,
  termNamePairs,
  termTreePairs,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax



-- ----------------------  Term

{-| Extract a term name. -}
termName :: TokenTree -> AbortOr String
termName (TreeL (TTerm _ [n])) = Right n
termName x = Left (AbortMissingTermName (show x), [])

{-| Extract a list of term names.
 
    >>> termNames . tokenTrees . tokens $ "/a /b /c"
    Right ["/a", "/b", "/c"]
-}
termNames :: [TokenTree] -> AbortOr [String]
termNames = mapM termName

{-| Extract a list of name-and-name pairs.
 
    >>> termNamePairs . tokenTrees . tokens $ "/a /x /b /y"
    Right [("/a", "/x"), ("/b", "/y")]
-}
termNamePairs :: [TokenTree] -> AbortOr [(String, String)]
termNamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           b'  <- termName b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop (a : _) = Left (AbortMissingTermName (show a), [])

{-| Extract a list of name-and-tree pairs.
 
    >>> termTreePairs . tokenTrees . tokens $ "/a 'A3' /b 10"
    Right [("/a", TreeL (TWord 3 1 "A3")),
           ("/b", TreeL (TWord 7 0 "10"))]
-}
termTreePairs :: [TokenTree] -> AbortOr [Named TokenTree]
termTreePairs = loop where
    loop (a : b : xs) =
        do a'  <- termName a
           xs' <- loop xs
           Right $ (a', b) : xs'
    loop [] = Right []
    loop (a : _) = Left (AbortMissingTermName (show a), [])



-- ----------------------  Calculation

-- -- | Calculator combining values into a single value
-- type Calc v = [v] -> v

-- -- | Set of operators
-- type Ripen x v = x -> [v] -> Calc v

-- --type TreeMap x y = Tree x -> Tree y

-- -- | Calculate a value of tree
-- crop
--   :: Ripen x v   -- ^ Set of Operators
--   -> Tree x      -- ^ Cropping target
--   -> Calc v      -- ^ Calculator
-- crop ripen tree1 arg = c tree1 [] where
--     c (TreeL x)        vs = ripen x vs arg
--     c (TreeB _ (x:xs)) vs = c x $ map (`c` vs) xs
--     c _ _ = undefined

-- gather a crop
-- harvest a crop
