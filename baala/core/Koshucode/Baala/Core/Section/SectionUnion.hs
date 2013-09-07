{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Koshucode.Baala.Core.Section.SectionUnion
( sectionUnion,
  sectionUse
) where

import Data.Monoid
import qualified Koshucode.Baala.Core.Section.Section as C

instance Monoid (C.Section c) where
    mempty  = C.emptySection
    mappend = sectionUnion

sectionUnion :: C.Section c -> C.Section c -> C.Section c
sectionUnion s1 s2 =
    s1 { C.sectionName   = Nothing
       , C.sectionImport = []
       , C.sectionExport = union C.sectionExport
       , C.sectionAssert = union C.sectionAssert
       , C.sectionRelmap = union C.sectionRelmap
       , C.sectionJudge  = union C.sectionJudge
       } where union f = f s1 ++ f s2

sectionUse :: C.Section c -> C.Section c -> C.Section c
sectionUse s1 s2 =
    s1 { C.sectionName   = Nothing
       , C.sectionImport = []
       , C.sectionRelmap = union C.sectionRelmap
       } where union f = f s1 ++ f s2

