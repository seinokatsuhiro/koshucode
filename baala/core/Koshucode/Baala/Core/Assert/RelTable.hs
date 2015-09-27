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
import qualified Koshucode.Baala.Data         as D



-- --------------------------------------------  Rel table

relTable :: (B.Write c, D.CRel c) => [D.ShortDef] -> D.Rel c -> String
relTable sh = unlines . relTableLines sh

relTableLines :: (B.Write c, D.CRel c) => [D.ShortDef] -> D.Rel c -> [String]
relTableLines sh r = render $ relCells 2 size [] text where
    text = relText sh r
    size = maxTermSize text

relCells :: Int -> TermSize -> D.TermPath -> D.RelText -> [[B.Cell]]
relCells pad m path (D.Rel he bo) = table where
    table = let ns = D.headNames he
                h  = map (text . D.showTermName) ns
            in h : map rule h : map (tuple ns) bo
    tuple ns cs = map content $ zip ns cs
    content (n, c) = case c of
                       D.MonoNest r -> texts $ render $ relCells pad m path' r
                       D.MonoTerm s -> width $ text s
        where path' = n : path
              width cell = let w = Map.findWithDefault 0 path' m
                           in cell { B.cellWidth = pad + w }

    texts  = B.textBlockCellPlus 1 B.Front
    text   = B.textCell B.Front
    rule _ = B.textRuleCell '-'

relText :: (B.Write c, D.CRel c) => [D.ShortDef] -> D.Rel c -> D.RelText
relText sh (D.Rel he bo) = D.Rel he $ map (map content) bo where
    content c | D.isRel c  = D.MonoNest $ relText sh $ D.gRel c
              | otherwise  = D.MonoTerm $ B.writeStringWith (D.shortText sh) c

render :: [[B.Cell]] -> [String]
render = B.squeezeEmptyLines . B.renderTable " " . B.alignTable



-- --------------------------------------------  Term Map

type TermMap a = Map.Map D.TermPath a
type TermSize = TermMap Int

maxTermSize :: D.RelText -> TermSize
maxTermSize = termMap D.gMonoNest (B.stringWidth . D.gMonoTerm) max

termMap :: forall a. forall c.
    (c -> D.Rel c) -> (c -> a) -> (a -> a -> a) -> D.Rel c -> TermMap a
termMap gRel from f (D.Rel he bo) = accum [] ts bo Map.empty where

    ts = D.typeTerms $ D.headType he

    accum :: D.TermPath -> [D.NamedType] -> [[c]] -> B.Map (TermMap a)
    accum path ts1 bo1 m = foldr (column path) m $ zip ts1 (List.transpose bo1)

    column :: D.TermPath -> (D.NamedType, [c]) -> B.Map (TermMap a)
    column path ((n, D.TypeRel ts2), cs2) m = accum (n : path) ts2 (bodies cs2) m
    column path ((n, _)            , cs2) m = foldr (add $ n : path) m cs2

    add :: D.TermPath -> c -> B.Map (TermMap a)
    add path c m = Map.insertWith f path (from c) m

    bodies :: [c] -> [[c]]
    bodies = concatMap (D.relBody . gRel)

