{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Builtin.Term
( termnames,
  --termname2,
  termnamePairs,
  termTreePairs,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------  Term

{-| Extract a term name. -}
termname :: B.TokenTree -> B.AbortTokens B.Termname
termname (B.TreeL (B.TTerm _ [n])) = Right n
termname tree = Left (B.AbortMissingTermname "",
                      B.treeTokens tree)

{-| Extract a list of term names.
 
    >>> termnames . B.tokenTrees . B.tokens $ "/a /b /c"
    Right ["/a", "/b", "/c"]
-}
termnames :: [B.TokenTree] -> B.AbortTokens [B.Termname]
termnames trees =
    case mapM termname trees of
      Right ns -> Right ns
      Left  _  -> Left (B.AbortMissingTermname "",
                        B.treesTokens trees)

{-| Extract a list of name-and-name pairs.
 
    >>> termnamePairs . B.tokenTrees . B.tokens $ "/a /x /b /y"
    Right [("/a", "/x"), ("/b", "/y")]
-}
termnamePairs :: [B.TokenTree] -> B.AbortTokens [(B.Termname, B.Termname)]
termnamePairs = loop where
    loop (a : b : xs) =
        do a'  <- termname a
           b'  <- termname b
           xs' <- loop xs
           Right $ (a', b') : xs'
    loop [] = Right []
    loop xs = Left (B.AbortMissingTermname "",
                    B.treesTokens xs)

{-| Extract a list of name-and-tree pairs.
 
    >>> termTreePairs . B.tokenTrees . B.tokens $ "/a 'A3 /b 10 /c"
    Right [ ("/a", TreeB 1 [ TreeL (TWord 3 0 "'")
                          , TreeL (TWord 4 0 "A3") ])
          , ("/b", TreeL (TWord 8 0 "10"))
          , ("/c", TreeB 1 []) ]
  -}
termTreePairs :: [B.TokenTree] -> B.AbortTokens [B.Named B.TokenTree]
termTreePairs xs =
    case C.litNamedTrees xs of
      Right x -> Right x
      Left a  -> Left (a, B.treesTokens xs)

