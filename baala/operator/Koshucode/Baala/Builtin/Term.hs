{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Builtin.Term
( termnames,
  termname2,
  termnamePairs,
  termTreePairs,
) where

import qualified Koshucode.Baala.Base as B



-- ----------------------  Term

{-| Extract a term name. -}
termname :: B.TokenTree -> B.AbortOr String
termname (B.TreeL (B.TTerm _ [n])) = Right n
termname tree = Left (B.AbortMissingTermname "",
                      [], B.treeTokens tree)

termname2 :: [B.TokenTree] -> B.AbortOr (String, String)
termname2 [B.TreeL (B.TTerm _ [n1]), B.TreeL (B.TTerm _ [n2])] =
    Right (n1, n2)
termname2 trees = Left (B.AbortMissingTermname "",
                        [], B.treesTokens trees)

{-| Extract a list of term names.
 
    >>> termnames . B.tokenTrees . B.tokens $ "/a /b /c"
    Right ["/a", "/b", "/c"]
-}
termnames :: [B.TokenTree] -> B.AbortOr [String]
termnames trees =
    case mapM termname trees of
      Right ns -> Right ns
      Left  _  -> Left (B.AbortMissingTermname "",
                        [], B.treesTokens trees)

{-| Extract a list of name-and-name pairs.
 
    >>> termnamePairs . tokenTrees . tokens $ "/a /x /b /y"
    Right [("/a", "/x"), ("/b", "/y")]
-}
termnamePairs :: [B.TokenTree] -> B.AbortOr [(String, String)]
termnamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termname a
           b'  <- termname b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop xs = Left (B.AbortMissingTermname "",
                    [], B.treesTokens xs)

{-| Extract a list of name-and-tree pairs.
 
    >>> termTreePairs . tokenTrees . tokens $ "/a 'A3' /b 10"
    Right [("/a", TreeL (TWord 3 1 "A3")),
           ("/b", TreeL (TWord 7 0 "10"))]
-}
termTreePairs :: [B.TokenTree] -> B.AbortOr [B.Named B.TokenTree]
termTreePairs = loop where
    loop (a : b : xs) =
        do a'  <- termname a
           xs' <- loop xs
           Right $ (a', b) : xs'
    loop [] = Right []
    loop xs = Left (B.AbortMissingTermname "",
                    [], B.treesTokens xs)

