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

relTable :: (B.Write c, C.CRel c) => [B.ShortDef] -> B.Rel c -> String
relTable sh = unlines . relTableLines sh

relTableLines :: (B.Write c, C.CRel c) => [B.ShortDef] -> B.Rel c -> [String]
relTableLines sh r = render $ relCells 2 size [] text where
    text = relText sh r
    size = maxTermSize text

relCells :: Int -> TermSize -> B.TermPath -> B.RelText -> [[B.Cell]]
relCells pad m path (B.Rel he bo) = table where
    table = let ns = B.headNames he
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

relText :: (B.Write c, C.CRel c) => [B.ShortDef] -> B.Rel c -> B.RelText
relText sh (B.Rel he bo) = B.Rel he $ map (map content) bo where
    content c | C.isRel c  = B.MonoNest $ relText sh $ C.gRel c
              | otherwise  = B.MonoType $ show $ B.write (B.shortText sh) c

render :: [[B.Cell]] -> [String]
render = B.squeezeEmptyLines . B.renderTable " " . B.alignTable



-- --------------------------------------------  Term Map

type TermMap a = Map.Map B.TermPath a
type TermSize = TermMap Int

maxTermSize :: B.RelText -> TermSize
maxTermSize = termMap B.gMonoNest (length . B.gMonoType) max

termMap :: forall a. forall c.
    (c -> B.Rel c) -> (c -> a) -> (a -> a -> a) -> B.Rel c -> TermMap a
termMap gRel from f (B.Rel he bo) = accum [] ts bo Map.empty where

    ty = B.termsToType $ B.headTerms he
    ts = B.typeTerms ty

    accum :: B.TermPath -> [B.NamedType] -> [[c]] -> B.Map (TermMap a)
    accum path ts1 bo1 m = foldr (column path) m $ zip ts1 (List.transpose bo1)

    column :: B.TermPath -> (B.NamedType, [c]) -> B.Map (TermMap a)
    column path ((n, B.TypeRel ts2), cs2) m = accum (n : path) ts2 (bodies cs2) m
    column path ((n, _)            , cs2) m = foldr (add $ n : path) m cs2

    add :: B.TermPath -> c -> B.Map (TermMap a)
    add path c m = Map.insertWith f path (from c) m

    bodies :: [c] -> [[c]]
    bodies = concatMap (B.relBody . gRel)

