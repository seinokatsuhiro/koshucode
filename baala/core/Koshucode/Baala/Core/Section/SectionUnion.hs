{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Core.Section.SectionUnion
( sectionUnion
, sectionUse
) where

import Data.Monoid
import Koshucode.Baala.Core.Section.Section

instance Monoid (Section v) where
    mempty  = emptySection
    mappend = sectionUnion

sectionUnion :: Section v -> Section v -> Section v
sectionUnion s1 s2 =
    s1 { sectionName   = Nothing
       , sectionImport = []
       , sectionExport = union sectionExport
       , sectionAssert = union sectionAssert
       , sectionRelmap = union sectionRelmap
       , sectionJudge  = union sectionJudge
       } where union f = f s1 ++ f s2

sectionUse :: Section v -> Section v -> Section v
sectionUse s1 s2 =
    s1 { sectionName   = Nothing
       , sectionImport = []
       , sectionRelmap = union sectionRelmap
       } where union f = f s1 ++ f s2

