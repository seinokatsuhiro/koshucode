{-# OPTIONS_GHC -Wall #-}

{-| Data structure for relational calculations.
    There are three types of section:
    (1) /editing sections/ that output judges and read other sections,
    (2) /library sections/ that make relmaps reusable,
    (3) /data sections/ that provide data.
  -}

module Koshucode.Baala.Core.Section.Section
(
  -- * Process
  -- $Process

  -- * Section
  Section (..),

  -- * Constructors
  makeEmptySection,
  emptySection,
  dataSection,
) where

import qualified Data.Monoid                 as M
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core.Relmap as C
import qualified Koshucode.Baala.Core.Assert as C

data Section c = Section {
      sectionName     :: Maybe String           -- ^ Section name
    , sectionImport   :: [Section c]            -- ^ Importing section
    , sectionExport   :: [String]               -- ^ Exporting relmap names
    , sectionShort    :: [B.Named String]       -- ^ Prefix for short signs
    , sectionAssert   :: [C.Assert c]           -- ^ Assertions of relmaps
    , sectionRelmap   :: [B.Named (C.Relmap c)] -- ^ Relmaps and its name
    , sectionJudge    :: [B.Judge c]            -- ^ Affirmative or denial judgements
    , sectionViolate  :: [B.Judge c]            -- ^ Violated judgements, i.e., result of @|=V@
    , sectionResource :: String                 -- ^ Resource name
    , sectionCons     :: C.RelmapCons c         -- ^ Relmap constructor for this section
    } deriving (Show)

instance (Ord c, B.Pretty c) => B.Pretty (Section c) where
    doc sec = dSection where
        dSection = B.docv [dRelmap, dAssert, dJudge]
        dRelmap  = B.docv $ map docRelmap $ sectionRelmap sec
        docRelmap (n,m) = B.docZero (n ++ " :") B.<+> B.doc m B.$$ B.doc ""
        dJudge   = B.docv $ sectionJudge sec
        dAssert  = B.docv $ sectionAssert sec

instance M.Monoid (Section c) where
    mempty  = emptySection
    mappend = sectionUnion

sectionUnion :: Section c -> Section c -> Section c
sectionUnion s1 s2 =
    s1 { sectionName   = Nothing
       , sectionImport = []
       , sectionExport = union sectionExport
       , sectionAssert = union sectionAssert
       , sectionRelmap = union sectionRelmap
       , sectionJudge  = union sectionJudge
       } where union f = f s1 ++ f s2



-- ----------------------  Constructors

{-| Make empty section that has a given constructor. -}
makeEmptySection :: C.RelmapCons c -> Section c
makeEmptySection = Section Nothing [] [] [] [] [] [] [] ""

{-| Section that has no contents. -}
emptySection :: Section c
emptySection = makeEmptySection $ C.relmapCons []

{-| Section that has only judgements. -}
dataSection :: [B.Judge c] -> Section c
dataSection js = emptySection { sectionJudge = js }



-- ----------------------
{- $Process
  
   Section is constructed using following steps.
   
   [1. @??? -> String@]
       Get string from something.
   
   [2. @String -> \[String\]@]
       Split string into lines.
   
   [3. @\[String\] -> \[Token\]@]
       Split line-breaked strings into tokens.
  
   [4. @\[Token\] -> \[\[Token\]\]@]
       Collect tokens for clauses.
  
   [5. @\[\[Token\]\] -> \[Clause\]@]
       Classify tokens.
  
   [6. @\[Clause\] -> Section a@]
       Make section from list of clauses.

-}
