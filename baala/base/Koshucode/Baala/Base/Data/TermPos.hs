{-# OPTIONS_GHC -Wall #-}

{-| Functions for term position -}

module Koshucode.Baala.Base.Data.TermPos
( -- * Datatype
  TermPos (..),

  -- * Construction
  posHere,
  posFor,
  posFrom,
  posTo,

  -- * Selection
  posExist,
  posInnerNames,
  posOuterNames,

  -- * Other functions
  posSortByName,
  posSortByIndex,
  posPick,
  posCut,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Relhead as B



-- ----------------------  Type

-- | Position of flat term
data TermPos = TermPos
    { posName   :: B.TermName  -- ^ Name of term
    , posIndex  :: Int         -- ^ Position
    } deriving (Show, Eq, Ord)

instance B.Name TermPos where
    name = posName



-- ----------------------  Construction

posHere :: B.Relhead -> [B.TermName] -> ([TermPos], [Bool])
posHere h ns = (ps, bs) where
    ps = h `posFor` ns
    bs = map posExist ps

-- | Positions of given names in a head
--
--   >>> B.headFrom ["/a", "/b", "/c"] `posFor` ["/b", "/c", "/d"]
--   [TermPos "/b" 1, TermPos "/c" 2, TermPos "/d" (-1)]
posFor :: B.Relhead -> [B.TermName] -> [TermPos]
posFor h ns = zipWith TermPos ns $ flatIndex ns where
    flatIndex = withSingleton $ B.headIndex h
    withSingleton f = map head . f . map B.singleton

-- | Positions of given (sub)head in a head
--
--   >>> B.headFrom ["/a", "/b", "/c"] `posFrom` B.headFrom ["/b", "/c", "/d"]
--   [TermPos "/b" 1, TermPos "/c" 2, TermPos "/d" (-1)]
posFrom :: B.Relhead -> B.Relhead -> [TermPos]
posFrom h1 h2 = h1 `posFor` B.headNames h2

-- | /h1/ @\`posTo\`@ /h2/ @==@ /h2/ @\`posFrom\`@ /h1/
posTo :: B.Relhead -> B.Relhead -> [TermPos]
posTo h1 h2 = posFrom h2 h1



-- ----------------------  Functions

posExist :: TermPos -> Bool
posExist pos = posIndex pos >= 0

-- | Pick inner part.
--
--   >>> posInnerNames [TermPos "/a" (-1), TermPos "/b" 1, TermPos "/c" 2]
--   ["/b", "/c"]
posInnerNames :: [TermPos] -> [B.TermName]
posInnerNames = posFilterNames posExist

-- | Pick outer part.
--
--   >>> posOuterNames [TermPos "/a" (-1), TermPos "/b" 1, TermPos "/c" 2]
--   ["/a"]
posOuterNames :: [TermPos] -> [B.TermName]
posOuterNames = posFilterNames (not . posExist)

posFilterNames :: (TermPos -> Bool) -> [TermPos] -> [B.TermName]
posFilterNames p = map posName . filter p

-- | Sort list of 'TermPos' by the name.
--
--   >>> posSortByName [TermPos "/y" 2, TermPos "/x" 0, TermPos "/z" 1]
--   [TermPos "/x" 0, TermPos "/y" 2, TermPos "/z" 1]
posSortByName :: B.Map [TermPos]
posSortByName = List.sortBy ord where
    ord p1 p2 = compare (posName p1) (posName p2)

-- | Sort list of 'TermPos' by the index.
--
--   >>> posSortByIndex [TermPos "/y" 2, TermPos "/x" 0, TermPos "/z" 1]
--   [TermPos "/x" 0, TermPos "/z" 1, TermPos "/y" 2]
posSortByIndex :: B.Map [TermPos]
posSortByIndex = List.sortBy ord where
    ord p1 p2 = compare (posIndex p1) (posIndex p2)

-- | Pick contents by positions.
--
--   >>> posPick [TermPos "/b" 1, TermPos "/c" 2] "abcd"
--   "bc"
posPick :: [TermPos] -> B.Map [c]
posPick = B.snipFrom . map posIndex

-- | Cut contents by positions.
--
--   >>> posCut [TermPos "/b" 1, TermPos "/c" 2] "abcd"
--   "ad"
posCut :: [TermPos] -> B.Map [c]
posCut = B.snipOff . map posIndex

