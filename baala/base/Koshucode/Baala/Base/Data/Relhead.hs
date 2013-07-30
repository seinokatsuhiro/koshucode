{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Heading
  Relhead (..),
  headFrom,
  headNames,
  headDegree,
  headChange,
  headTermExist,
  headTermCheck,
  headExistTerms,
  headNonExistTerms,

  -- * Index
  headIndex,
  headIndex1,
  posOf,
  posFrom,

  -- * Monoid
  mempty,
  mappend,
) where

import Data.Monoid
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Relterm
import Koshucode.Baala.Base.Data.TermPos



-- ---------------------- Heading

-- | Heading of relation as a list of terms
data Relhead = Relhead {
      headTerms :: [Relterm]
    } deriving (Show, Eq, Ord)

instance Monoid Relhead where
    mempty = Relhead []
    mappend (Relhead t1) (Relhead t2) =
        Relhead $ unionUp t1 t2

instance Pretty Relhead where
    doc (Relhead ts) = doch $ map doc ts

{-| Make head from term names.

    >>> headFrom ["/a", "/b"]
    Relhead [Term "/a", Term "/b"]
-}
headFrom :: [String] -> Relhead
headFrom ns = Relhead $ map Term ns

{-| List of term names.

    >>> headNames $ headFrom ["/a", "/b"]
    ["/a", "/b"]
 -}
headNames :: Relhead -> [String]
headNames (Relhead ts) = names ts

{-| Number of terms.

    >>> headDegree $ headFrom ["/a", "/b"]
    2
 -}
headDegree :: Relhead -> Int
headDegree = length . headTerms

{-| Reconstruct head. -}
headChange :: (Map [String]) -> Map Relhead
headChange f (Relhead ts) = headFrom $ f $ names ts

headTermExist :: Relhead -> [String] -> Bool
headTermExist h ns = headTermCheck h (map Yes ns)

headTermCheck :: Relhead -> [YesNo String] -> Bool
headTermCheck (Relhead ts) = all (termCheck1 ts)

termCheck1 :: [Relterm] -> YesNo String -> Bool
termCheck1 ts (Yes n) = let [i] = termIndex ts [n] in i >= 0
termCheck1 ts (No  n) = let [i] = termIndex ts [n] in i == -1

headNonExistTerms :: Relhead -> Map [String]
headNonExistTerms (Relhead ts) = filter (termCheck1 ts . No)

headExistTerms :: Relhead -> Map [String]
headExistTerms (Relhead ts) = filter (termCheck1 ts . Yes)



-- ----------------------  Index

headIndex :: Relhead -> [[String]] -> [[Int]]
headIndex h n = termsIndex (headTerms h) n

headIndex1 :: Relhead -> [String] -> [Int]
headIndex1 h n = termIndex (headTerms h) n

{-| Positions of given names in a head -}
posOf :: Relhead -> [[String]] -> [TermPos]
posOf h1 ns = termPoss ns $ headIndex h1 ns

{-| Positions of given (sub)head in a head -}
posFrom :: Relhead -> Relhead -> [TermPos]
posFrom h1 h2 = h1 `posOf` n2 where
    n2 = map singleton (headNames h2)

termPoss :: [[String]] -> [[Int]] -> [TermPos]
termPoss ns ps = zipWith TermPos ns2 ps2 where
    ns2 = map head ns
    ps2 = map head ps

