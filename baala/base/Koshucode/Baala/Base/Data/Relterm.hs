{-# OPTIONS_GHC -Wall #-}

{-| Terms in heading of relation. -}

module Koshucode.Baala.Base.Data.Relterm
( Relterm (..),
  termsIndex,
  termIndex,
)
where

import Koshucode.Baala.Base.Prelude

{-| Term in heading of relation -}
data Relterm
    = Term String             -- ^ For non-relation
    | Nest String [Relterm]   -- ^ For relation
      deriving (Show, Eq, Ord)

instance Name Relterm where
    name (Term s)   = s
    name (Nest s _) = s

instance Pretty Relterm where
    doc (Term n)    = doc n
    doc (Nest n xs) = docWraps "(" ")" (doch $ doc n : map doc xs)

{-| Term path to term position

    >>> termIndex ["/b"] [Term "/a", Term "/b", Term "/c"]
    [1]

    >>> termIndex ["/e"] [Term "/a", Term "/b", Term "/c"]
    [-1]

    >>> termIndex ["/r", "/b"] [Nest "/r" [Term "/a", Term "/b"]]
    [0, 1]  -}
termIndex :: [Relterm] -> [String] -> [Int]
termIndex ts p = loop ts p 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop (Term n1 : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
    loop (Nest n1 ts' : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)

termsIndex :: [Relterm] -> [[String]] -> [[Int]]
termsIndex ts = map (termIndex ts)

