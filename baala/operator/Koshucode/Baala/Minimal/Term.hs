{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Minimal.Term
( termNames,
  termName2,
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

termName2 :: [TokenTree] -> AbortOr (String, String)
termName2 [TreeL (TTerm _ [n1]), TreeL (TTerm _ [n2])] = Right (n1, n2)
termName2 x = Left (AbortMissingTermName (show x), [])

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

