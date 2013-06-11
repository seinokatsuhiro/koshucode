{-# OPTIONS_GHC -Wall #-}

-- | Terms in heading of relation

module Koshucode.Baala.Base.Data.Rel.Relterm
( Relterm (..)
, termLook
, termLook1
)
where

import Koshucode.Baala.Base.Prelude

-- | Term in heading of relation
data Relterm
    = Term String
    | Nest String [Relterm]
      deriving (Show, Eq, Ord)

instance Name Relterm where
    name (Term s)   = s
    name (Nest s _) = s

instance Pretty Relterm where
    doc (Term n)    = text n
    doc (Nest n xs) = docParen (hsep $ text n : map doc xs)

-- | Term path to term position
termLook1 :: [String] -> [Relterm] -> [Int]
termLook1 path ts = loop ts path 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop (Term n1 : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
    loop (Nest n1 ts' : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)

termLook :: [[String]] -> [Relterm] -> [[Int]]
termLook ns ts = map look1 ns where
    look1 n = termLook1 n ts

