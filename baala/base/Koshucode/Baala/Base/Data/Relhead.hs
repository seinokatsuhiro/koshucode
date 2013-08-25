{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Type
  Relhead (..),
  headFrom,
  headCons,
  headChange,
  headNames,
  headDegree,

  -- * Other functions
  headTermExist,
  headTermCheck,
  headExistTerms,
  headNonExistTerms,
  headIndex,
  headIndex1,

  -- * Monoid
  mempty,
  mappend,
) where

import Data.Monoid
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data.Relterm



-- ---------------------- Heading

{-| Heading of relation as a list of terms -}
data Relhead = Relhead {
      headTerms :: [Relterm]
    } deriving (Show, Eq, Ord)

instance Monoid Relhead where
    mempty = Relhead []
    mappend (Relhead t1) (Relhead t2) =
        Relhead $ unionUp t1 t2

{-| >>> doc $ headFrom ["/a", "/b"]
    /a : /b  -}
instance Pretty Relhead where
    doc (Relhead ts) = docColon ts

{-| Make head from termnames.

    >>> headFrom ["/a", "/b"]
    Relhead [Term "/a", Term "/b"]  -}
headFrom :: [String] -> Relhead
headFrom ns = Relhead $ map Term ns

{-| Add term to head.

    >>> let h = headFrom ["/a", "/b"] in headCons "/c" h
    Relhead [Term "/c", Term "/a", Term "/b"]  -}
headCons :: String -> Map Relhead
headCons n (Relhead ns) = Relhead $ Term n : ns

{-| Reconstruct head.

    >>> let h = headFrom ["/a", "/b"] in headChange reverse h
    Relhead [Term "/b", Term "/a"]  -}
headChange :: (Map [String]) -> Map Relhead
headChange f (Relhead ts) = headFrom $ f $ names ts

{-| List of term names.

    >>> let h = headFrom ["/a", "/b"] in headNames h
    ["/a", "/b"]  -}
headNames :: Relhead -> [String]
headNames (Relhead ts) = names ts

{-| Number of terms.

    >>> let h = headFrom ["/a", "/b"] in headDegree h
    2  -}
headDegree :: Relhead -> Int
headDegree = length . headTerms


-- ----------------------  Other functions


headTermExist :: Relhead -> [String] -> Bool
headTermExist h ns = headTermCheck h (map Yes ns)

{-| Check term existences.

    >>> let h = headFrom ["/a", "/b"] in headTermCheck h [Yes "/a", No "/c"]
    True

    >>> let h = headFrom ["/a", "/b"] in headTermCheck h [Yes "/a", No "/b"]
    False  -}
headTermCheck :: Relhead -> [YesNo String] -> Bool
headTermCheck (Relhead ts) = all (termCheck1 ts)

termCheck1 :: [Relterm] -> YesNo String -> Bool
termCheck1 ts (Yes n) = let [i] = termIndex ts [n] in i >= 0
termCheck1 ts (No  n) = let [i] = termIndex ts [n] in i == -1

{-| Filter keeping terms that exist in head.

    >>> let h = headFrom ["/a", "/b"] in headExistTerms h ["/a", "/c"]
    ["/a"]  -}
headExistTerms :: Relhead -> Map [String]
headExistTerms (Relhead ts) = filter (termCheck1 ts . Yes)

{-| Filter dropping terms that exist in head.

    >>> let h = headFrom ["/a", "/b"] in headNonExistTerms h ["/a", "/c"]
    ["/c"]  -}
headNonExistTerms :: Relhead -> Map [String]
headNonExistTerms (Relhead ts) = filter (termCheck1 ts . No)

{-| Index of terms.

    >>> let h = headFrom ["/a", "/b"] in headIndex h [["/a"], ["/b"], ["/c"]]
    [[0], [1], [-1]]  -}
headIndex :: Relhead -> [[String]] -> [[Int]]
headIndex (Relhead ts) n = termsIndex ts n

{-| Index of terms.

    >>> let h = headFrom ["/a", "/b"] in headIndex1 h ["/a"]
    [0]  -}
headIndex1 :: Relhead -> [String] -> [Int]
headIndex1 (Relhead ts) n = termIndex ts n

