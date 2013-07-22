{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Heading
  Relhead (..),
  headFrom,
  headNames,
  headDegree,
  headChange,

  -- * Index
  headIndex,
  headIndex1,
  posOf,
  posFrom,

  -- * Position
  TermPos (..),
  posPoss,
  csPick,
  csCut,
  termsInner,
  termsOuter,

  -- * Monoid
  mempty,
  mappend,
) where

import Data.Monoid
import Koshucode.Baala.Base.Prelude.Position
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Relterm



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
headChange f (Relhead h) = headFrom $ f $ names h



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



-- ----------------------  Term position

{-| Term position -}
data TermPos = TermPos
    { posName   :: String    -- ^ Term name
    , posIndex  :: Int       -- ^ Position
    } deriving (Show, Eq, Ord)

instance Name TermPos where
    name (TermPos n _) = n

{-| Indicies -}
posPoss  :: [TermPos] -> [Int]
posPoss  = map posIndex

{-| Pick an inner part. -}
termsInner :: [TermPos] -> [[String]]
termsInner = termsFilter isInner

{-| Pick an outer part. -}
termsOuter :: [TermPos] -> [[String]]
termsOuter = termsFilter isOuter

termsFilter :: (TermPos -> Bool) -> [TermPos] -> [[String]]
termsFilter p = map (singleton . name) . filter p

isInner, isOuter :: TermPos -> Bool
isInner (TermPos _ i)  =  i >= 0
isOuter (TermPos _ i)  =  i <  0

{-| Pick contents by positions. -}
csPick :: [TermPos] -> Map [c]
csPick  =  indexPick . map posIndex

{-| Cut contents by positions. -}
csCut  :: [TermPos] -> Map [c]
csCut   =  indexCut . map posIndex

