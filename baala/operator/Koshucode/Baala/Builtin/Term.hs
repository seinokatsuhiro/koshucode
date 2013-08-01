{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Builtin.Term
( termnames,
  termname2,
  termnamePairs,
  termTreePairs,
) where

import Koshucode.Baala.Base



-- ----------------------  Term

{-| Extract a term name. -}
termname :: TokenTree -> AbortOr String
termname (TreeL (TTerm _ [n])) = Right n
termname x = Left (AbortMissingTermname (show x), [])

termname2 :: [TokenTree] -> AbortOr (String, String)
termname2 [TreeL (TTerm _ [n1]), TreeL (TTerm _ [n2])] = Right (n1, n2)
termname2 x = Left (AbortMissingTermname (show x), [])

{-| Extract a list of term names.
 
    >>> termnames . tokenTrees . tokens $ "/a /b /c"
    Right ["/a", "/b", "/c"]
-}
termnames :: [TokenTree] -> AbortOr [String]
termnames = mapM termname

{-| Extract a list of name-and-name pairs.
 
    >>> termnamePairs . tokenTrees . tokens $ "/a /x /b /y"
    Right [("/a", "/x"), ("/b", "/y")]
-}
termnamePairs :: [TokenTree] -> AbortOr [(String, String)]
termnamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termname a
           b'  <- termname b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop (a : _) = Left (AbortMissingTermname (show a), [])

{-| Extract a list of name-and-tree pairs.
 
    >>> termTreePairs . tokenTrees . tokens $ "/a 'A3' /b 10"
    Right [("/a", TreeL (TWord 3 1 "A3")),
           ("/b", TreeL (TWord 7 0 "10"))]
-}
termTreePairs :: [TokenTree] -> AbortOr [Named TokenTree]
termTreePairs = loop where
    loop (a : b : xs) =
        do a'  <- termname a
           xs' <- loop xs
           Right $ (a', b) : xs'
    loop [] = Right []
    loop (a : _) = Left (AbortMissingTermname (show a), [])

