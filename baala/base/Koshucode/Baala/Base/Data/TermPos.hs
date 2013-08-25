{-# OPTIONS_GHC -Wall #-}

{-| Functions for term position -}

module Koshucode.Baala.Base.Data.TermPos
( -- * Type
  TermPos (..),

  -- * Functions
  posSortByName,
  posSortByIndex,
  posPick,
  posCut,
  posInner,
  posOuter,
  posExist,

  posNest,
  posFlat,
  posFrom,
  posHere,
) where

import qualified Data.List as List
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data.Relhead



-- ----------------------  Type

{-| Term position -}
data TermPos = TermPos
    { posName   :: String    -- ^ Termname
    , posIndex  :: Int       -- ^ Position
    } deriving (Show, Eq, Ord)

instance Name TermPos where
    name = posName



-- ----------------------  Functions

{-| Sort list of 'TermPos' by the name.

    >>> posSortByName [TermPos "/y" 2, TermPos "/x" 0, TermPos "/z" 1]
    [TermPos "/x" 0, TermPos "/y" 2, TermPos "/z" 1]  -}
posSortByName :: Map [TermPos]
posSortByName = List.sortBy ord where
    ord p1 p2 = compare (posName p1) (posName p2)

{-| Sort list of 'TermPos' by the index.

    >>> posSortByIndex [TermPos "/y" 2, TermPos "/x" 0, TermPos "/z" 1]
    [TermPos "/x" 0, TermPos "/z" 1, TermPos "/y" 2]  -}
posSortByIndex :: Map [TermPos]
posSortByIndex = List.sortBy ord where
    ord p1 p2 = compare (posIndex p1) (posIndex p2)

{-| Pick contents by positions.

    >>> posPick [TermPos "/b" 1, TermPos "/c" 2] "abcd"
    "bc"  -}
posPick  :: [TermPos] -> Map [c]
posPick  =  arrangePick . map posIndex

{-| Cut contents by positions.

    >>> posCut [TermPos "/b" 1, TermPos "/c" 2] "abcd"
    "ad"  -}
posCut   :: [TermPos] -> Map [c]
posCut   =  arrangeCut . map posIndex

{-| Pick inner part.

    >>> posInner [TermPos "/a" (-1), TermPos "/b" 1, TermPos "/c" 2]
    [["/b"], ["/c"]]  -}
posInner :: [TermPos] -> [[String]]
posInner = posFilter posExist

{-| Pick outer part.

    >>> posOuter [TermPos "/a" (-1), TermPos "/b" 1, TermPos "/c" 2]
    [["/a"]]  -}
posOuter :: [TermPos] -> [[String]]
posOuter = posFilter (not . posExist)

posFilter :: (TermPos -> Bool) -> [TermPos] -> [[String]]
posFilter p = map (singleton . posName) . filter p

posExist :: TermPos -> Bool
posExist pos = posIndex pos >= 0

--

{-| Positions of given names in a head -}
posNest :: Relhead -> [[String]] -> [TermPos]
posNest h1 ns = termPoss ns $ headIndex h1 ns

posFlat :: Relhead -> [String] -> [TermPos]
posFlat h ns = h `posNest` map singleton ns

{-| Positions of given (sub)head in a head -}
posFrom :: Relhead -> Relhead -> [TermPos]
posFrom h1 h2 = h1 `posNest` n2 where
    n2 = map singleton (headNames h2)

termPoss :: [[String]] -> [[Int]] -> [TermPos]
termPoss ns ps = zipWith TermPos ns2 ps2 where
    ns2 = map head ns
    ps2 = map head ps

posHere :: Relhead -> [String] -> ([TermPos], [Bool])
posHere h ns = let ps = h `posFlat` ns
                   hs = map posExist ps
               in (ps, hs)

