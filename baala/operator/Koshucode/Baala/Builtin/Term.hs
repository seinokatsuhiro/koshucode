{-# OPTIONS_GHC -Wall #-}

{-| Parsing list of terms. -}

module Koshucode.Baala.Builtin.Term
( termnames,
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
 
    >>> termnames . B.tt $ "/a /b /c"
    Right ["/a", "/b", "/c"]
-}
termnames :: [B.TokenTree] -> B.AbortTokens [B.Termname]
termnames trees =
    case mapM termname trees of
      Right ns -> Right ns
      Left  _  -> Left (B.AbortMissingTermname "",
                        B.treesTokens trees)

{-| Extract a list of name-and-name pairs.
 
    >>> termnamePairs . B.tt $ "/a /x /b /y"
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
 
    >>> termTreePairs . B.tt $ "/a 'A3 /b 10 /c"
    Right [ ("/a", TreeL (TWord 3 1 "A3"))
          , ("/b", TreeL (TWord 7 0 "10"))
          , ("/c", TreeB 1 []) ]
  -}
termTreePairs :: [B.TokenTree] -> B.AbortTokens [B.Named B.TokenTree]
termTreePairs xs =
    case C.litNamedTrees xs of
      Right x -> Right x
      Left a  -> Left (a, B.treesTokens xs)

