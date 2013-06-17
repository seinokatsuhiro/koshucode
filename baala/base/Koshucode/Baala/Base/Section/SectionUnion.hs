{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Section.SectionUnion
( sectionUnion
, sectionUse
) where
import Koshucode.Baala.Base.Section.Section

sectionUnion :: Section v -> Section v -> Section v
sectionUnion m1 m2 =
    m1 { sectionName   = Nothing
       , sectionImport = []
       , sectionExport = union sectionExport
       , sectionAssert = union sectionAssert
       , sectionRelmap = union sectionRelmap
       , sectionJudge  = union sectionJudge
       } where union f = f m1 ++ f m2

sectionUse :: Section v -> Section v -> Section v
sectionUse m1 m2 =
    m1 { sectionName   = Nothing
       , sectionImport = []
       , sectionRelmap = union sectionRelmap
       } where union f = f m1 ++ f m2
        

