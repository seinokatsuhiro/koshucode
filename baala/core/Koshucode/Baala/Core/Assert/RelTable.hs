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

relTable :: (B.Pretty c, C.CRel c) => B.Rel c -> String
relTable = unlines . relTableLines

relTableLines :: (B.Pretty c, C.CRel c) => B.Rel c -> [String]
relTableLines r = render $ relCells 2 [] rwidth rtext where
    rtext    = relText r
    rwidth   = maxTermSize rtext

relCells :: Int -> B.TermPath -> TermSize -> B.RelText -> [[B.Cell]]
relCells pad path m (B.Rel (B.Relhead ts) bo) = table where
    table = let ns = map B.termName ts
                h  = map (text . B.showTermName) ns
            in h : map rule h : map (tuple ns) bo
    tuple ns cs = map content $ zip ns cs
    content (n, c) = case c of
                       B.MonoNest r -> texts $ render $ relCells pad path' m r
                       B.MonoType s -> width $ text s
        where path' = n : path
              width cell = cell { B.cellWidth = pad + Map.findWithDefault 0 path' m }

    texts  = B.textBlockCellPlus 1 B.Front
    text   = B.textCell B.Front
    rule _ = B.textRuleCell '-'

relText :: (B.Pretty c, C.CRel c) => B.Rel c -> B.RelText
relText (B.Rel he bo) = B.Rel he $ map (map content) bo where
    content c | C.isRel c = B.MonoNest $ relText $ C.gRel c
              | otherwise = B.MonoType $ show $ B.doc c

render :: [[B.Cell]] -> [String]
render = B.squeezeEmptyLines . B.renderTable " " . B.alignTable



-- --------------------------------------------  Term Map

type TermMap a = Map.Map B.TermPath a
type TermSize = TermMap Int

maxTermSize :: B.RelText -> TermSize
maxTermSize = termMap B.gMonoNest (length . B.gMonoType) max

termMap :: (c -> B.Rel c) -> (c -> a) -> (a -> a -> a) -> B.Rel c -> TermMap a
termMap gRel from f (B.Rel (B.Relhead ts) bo) =
    accum [] Map.empty $ zip ts $ List.transpose bo
    where
      accum  path = foldr $ column path
      column path (B.Relnest n ts2, cs) m = accum (n : path) m $ zip ts2 (trans cs)
      column path (B.Relterm n, cs)     m = foldr (add $ n : path) m cs
      add    path c m = Map.insertWith f path (from c) m
      trans = List.transpose . concatMap (B.relBody . gRel)
