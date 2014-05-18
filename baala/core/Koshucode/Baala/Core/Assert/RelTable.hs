{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert relation to text table.

module Koshucode.Baala.Core.Assert.RelTable
( relTable,
  relTableLines,
  relText,
) where

import qualified Data.List                    as List
import qualified Data.Map                     as Map
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Content as C



-- --------------------------------------------  Rel table

relTable :: (B.ShortDoc c, C.CRel c) => B.Rel c -> String
relTable = unlines . relTableLines

relTableLines :: (B.ShortDoc c, C.CRel c) => B.Rel c -> [String]
relTableLines r = render $ relCells 2 size [] text where
    text = relText r
    size = maxTermSize text

relCells :: Int -> TermSize -> B.TermPath -> B.RelText -> [[B.Cell]]
relCells pad m path (B.Rel (B.Relhead ts) bo) = table where
    table = let ns = map B.termName ts
                h  = map (text . B.showTermName) ns
            in h : map rule h : map (tuple ns) bo
    tuple ns cs = map content $ zip ns cs
    content (n, c) = case c of
                       B.MonoNest r -> texts $ render $ relCells pad m path' r
                       B.MonoType s -> width $ text s
        where path' = n : path
              width cell = let w = Map.findWithDefault 0 path' m
                           in cell { B.cellWidth = pad + w }

    texts  = B.textBlockCellPlus 1 B.Front
    text   = B.textCell B.Front
    rule _ = B.textRuleCell '-'

relText :: (B.ShortDoc c, C.CRel c) => B.Rel c -> B.RelText
relText (B.Rel he bo) = B.Rel he $ map (map content) bo where
    content c | C.isRel c  = B.MonoNest $ relText $ C.gRel c
              | otherwise  = B.MonoType $ show $ B.shortDoc [] c

render :: [[B.Cell]] -> [String]
render = B.squeezeEmptyLines . B.renderTable " " . B.alignTable



-- --------------------------------------------  Term Map

type TermMap a = Map.Map B.TermPath a
type TermSize = TermMap Int

maxTermSize :: B.RelText -> TermSize
maxTermSize = termMap B.gMonoNest (length . B.gMonoType) max

termMap :: forall a. forall c.
    (c -> B.Rel c) -> (c -> a) -> (a -> a -> a) -> B.Rel c -> TermMap a
termMap gRel from f (B.Rel (B.Relhead ts) bo) = accum [] ts bo Map.empty where

    accum :: B.TermPath -> [B.Term] -> [[c]] -> B.Map (TermMap a)
    accum path ts1 bo1 m = foldr (column path) m $ zip ts1 (List.transpose bo1)

    column :: B.TermPath -> (B.Term, [c]) -> B.Map (TermMap a)
    column path (B.TermFlat n,     cs2) m = foldr (add $ n : path) m cs2
    column path (B.TermNest n ts2, cs2) m = accum (n : path) ts2 (bodies cs2) m

    add :: B.TermPath -> c -> B.Map (TermMap a)
    add path c m = Map.insertWith f path (from c) m

    bodies :: [c] -> [[c]]
    bodies = concatMap (B.relBody . gRel)

