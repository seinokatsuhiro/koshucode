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
import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Type         as T
import qualified Koshucode.Baala.Data         as D


-- --------------------------------------------  Rel table

-- | Render relation in table layout.
relTable :: (D.CRel c, B.MixEncode c) => [S.ShortDef String] -> T.Rel c -> String
relTable sh = unlines . relTableLines sh

-- | Render relation in table layout.
relTableLines :: (D.CRel c, B.MixEncode c) => [S.ShortDef String] -> T.Rel c -> [String]
relTableLines sh r = render $ relCells 2 size [] text where
    text = relText sh r
    size = maxTermSize text

relCells :: Int -> TermSize -> S.TermPath -> T.RelText -> [[B.Cell]]
relCells pad m path (T.Rel he bo) = table where
    table = let ns = T.getTermNames he
                h  = map (text . S.termNameString) ns
            in h : map rule h : map (tuple ns) bo
    tuple ns cs = map content $ zip ns cs
    content (n, c) = case c of
                       T.MonoNest r -> texts $ render $ relCells pad m path' r
                       T.MonoTerm s -> width $ text s
        where path' = n : path
              width cell = let w = Map.findWithDefault 0 path' m
                           in cell { B.cellWidth = pad + w }

    texts  = B.textBlockCellPlus 1 B.Front
    text   = B.textCell B.Front
    rule _ = B.textRuleCell '-'

-- | Convert contents of relation into strings.
relText :: (D.CRel c, B.MixEncode c) => [S.ShortDef String] -> T.Rel c -> T.RelText
relText sh (T.Rel he bo) = T.Rel he $ map (map content) bo where
    content c | D.isRel c  = T.MonoNest $ relText sh $ D.gRel c
              | otherwise  = T.MonoTerm $ B.mixToFlatString $ B.mixTransEncode (S.shortText sh) c

render :: [[B.Cell]] -> [String]
render = B.squeezeEmptyLines . B.renderTable " " . B.alignTable



-- --------------------------------------------  Term Map

type TermMap a = Map.Map S.TermPath a
type TermSize = TermMap Int

maxTermSize :: T.RelText -> TermSize
maxTermSize = termMap T.gMonoNest (O.stringWidth . T.gMonoTerm) max

termMap :: forall a. forall c.
    (c -> T.Rel c) -> (c -> a) -> (a -> a -> a) -> T.Rel c -> TermMap a
termMap gRel from f (T.Rel he bo) = accum [] ts bo Map.empty where

    ts = T.typeTerms $ T.headType he

    accum :: S.TermPath -> [T.TypeTerm] -> [[c]] -> O.Map (TermMap a)
    accum path ts1 bo1 m = foldr (column path) m $ zip ts1 (List.transpose bo1)

    column :: S.TermPath -> (T.TypeTerm, [c]) -> O.Map (TermMap a)
    column path ((n, T.TypeRel ts2), cs2) m = accum (n : path) ts2 (bodies cs2) m
    column path ((n, _)            , cs2) m = foldr (add $ n : path) m cs2

    add :: S.TermPath -> c -> O.Map (TermMap a)
    add path c m = Map.insertWith f path (from c) m

    bodies :: [c] -> [[c]]
    bodies = concatMap (T.relBody . gRel)

