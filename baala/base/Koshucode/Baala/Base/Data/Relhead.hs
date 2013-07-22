{-# OPTIONS_GHC #-}

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
  headPoss,
  posFrom,

  -- * Position
  TermPos (..),
  posPoss,
  possPick,
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
headPoss :: Relhead -> [[String]] -> [TermPos]
headPoss h1 ns = termPoss ns $ headIndex h1 ns

{-| Positions of given (sub)head in a head -}
posFrom :: Relhead -> Relhead -> [TermPos]
posFrom h1 h2 = headPoss h1 n2 where
    n2 = map singleton (headNames h2)

termPoss :: [[String]] -> [[Int]] -> [TermPos]
termPoss ns ps = zipWith TermPos ns2 ps2 where
    ns2 = map head ns
    ps2 = map head ps



-- ----------------------  Term position

{-| Term position -}
data TermPos = TermPos
    { termPosName :: String    -- ^ Term name
    , termPos     :: Int       -- ^ Position
    } deriving (Show, Eq, Ord)

instance Name TermPos where
    name (TermPos n _) = n

{-| Indicies -}
posPoss  :: [TermPos] -> [Int]
posPoss  = map termPos

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

-- -- | Split positions into outer and inner parts
-- posSplit :: [TermPos] -> ([TermPos], [TermPos])
-- posSplit ps = loop ps where
--     loop [] = ([], [])
--     loop (p : ps)
--         | outer p   = mapFst (p:) $ loop ps
--         | otherwise = mapSnd (p:) $ loop ps

{-| Pick values -}
possPick :: [TermPos] -> [v] -> [v]
possPick ps = indexPick (map termPos ps)

