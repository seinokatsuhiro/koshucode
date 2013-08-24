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
) where

import qualified Data.List as List
import Koshucode.Baala.Base.Prelude



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
posInner = posFilter isInner

{-| Pick outer part.

    >>> posOuter [TermPos "/a" (-1), TermPos "/b" 1, TermPos "/c" 2]
    [["/a"]]  -}
posOuter :: [TermPos] -> [[String]]
posOuter = posFilter isOuter

posFilter :: (TermPos -> Bool) -> [TermPos] -> [[String]]
posFilter p = map (singleton . posName) . filter p

isInner, isOuter :: TermPos -> Bool
isInner pos  =  posIndex pos >= 0
isOuter pos  =  posIndex pos <  0

