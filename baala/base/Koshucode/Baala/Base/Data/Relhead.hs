{-# OPTIONS_GHC #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( Relhead (..),
  TermPos,
  mempty, mappend,
  headFrom,
  headDegree, headNames,
  rehead,
  headPoss, headPosh,
  posPoss,
  possPick,
  possInner, possOuter,
) where

import Data.Monoid
import Koshucode.Baala.Base.Prelude.Position as Pos
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Relterm

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

-- | Make head from term names
headFrom :: [String] -> Relhead
headFrom ns = Relhead $ map Term ns

-- | List of term names
headNames :: Relhead -> [String]
headNames (Relhead ts) = names ts

-- | Number of terms
headDegree :: Relhead -> Int
headDegree = length . headTerms

-- | Reconstruct head
rehead :: ([String] -> [String]) -> Relhead -> Relhead
rehead f (Relhead h) = Relhead $ map Term $ f $ names h



-- ----------------------  Index

headIndex :: Relhead -> [[String]] -> [[Int]]
headIndex h n = termLook n $ headTerms h

-- | Positions of given names in a head
headPoss :: Relhead -> [[String]] -> [TermPos]
headPoss h1 ns = termPoss ns $ headIndex h1 ns

-- | Positions of given (sub)head in a head
headPosh :: Relhead -> Relhead -> [TermPos]
headPosh h1 h2 = headPoss h1 n2 where
    n2 = map singleton (headNames h2)

termPoss :: [[String]] -> [[Int]] -> [TermPos]
termPoss ns ps = zipWith pos ns2 ps2 where
    ns2 = map head ns
    ps2 = map head ps
    pos n p = TermPos p n



-- ----------------------  Term position

-- | Term position
data TermPos = TermPos {
      termPos     :: Int       -- ^ Position
    , termPosName :: String    -- ^ Term name
    } deriving (Show, Eq, Ord)

instance Name TermPos where
    name (TermPos _ n) = n

-- | Indicies
posPoss  :: [TermPos] -> [Int]
posPoss  = map termPos

-- | Pick an inner part.
possInner :: [TermPos] -> [[String]]
possInner ps = map (singleton . name) $ filter inner ps

-- | Pick an outer part.
possOuter :: [TermPos] -> [[String]]
possOuter ps = map (singleton . name) $ filter outer ps

inner, outer :: TermPos -> Bool
inner (TermPos i _) = i >= 0
outer (TermPos i _) = i < 0

-- -- | Split positions into outer and inner parts
-- posSplit :: [TermPos] -> ([TermPos], [TermPos])
-- posSplit ps = loop ps where
--     loop [] = ([], [])
--     loop (p : ps)
--         | outer p   = mapFst (p:) $ loop ps
--         | otherwise = mapSnd (p:) $ loop ps

-- | Pick values
possPick :: [TermPos] -> [v] -> [v]
possPick ps = Pos.indexPick (map termPos ps)

