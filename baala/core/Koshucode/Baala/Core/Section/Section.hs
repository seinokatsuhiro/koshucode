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

  -- * Selectors
  sectionLinkedAssert,

  -- * Constructors
  makeEmptySection,
  emptySection,
  dataSection,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap as C

data Section c = Section {
      sectionName     :: Maybe String           -- ^ Section name
    , sectionImport   :: [Section c]            -- ^ Importing section
    , sectionExport   :: [String]               -- ^ Exporting relmap names
    , sectionAssert   :: [C.Assert c]           -- ^ Assertions of relmaps
    , sectionRelmap   :: [B.Named (C.Relmap c)] -- ^ Relmaps and its name
    , sectionJudge    :: [B.Judge c]            -- ^ Here data
    , sectionResource :: String                 -- ^ Resource name
    , sectionCons     :: C.RelmapCons c         -- ^ Readers and writers for this section
    } deriving (Show)

instance (Ord c, B.Pretty c) => B.Pretty (Section c) where
    doc sec = dSection where
        dSection = B.docv [dRelmap, dAssert, dJudge]
        dRelmap  = B.docv $ map docRelmap $ sectionRelmap sec
        docRelmap (n,m) = B.docZero (n ++ " :") B.<+> B.doc m B.$$ B.doc ""
        dJudge   = B.docv $ sectionJudge sec
        dAssert  = B.docv $ sectionAssert sec



-- ----------------------  Selectors

{-| Select assertions like 'sectionAssert'.
    It returns relmap-liked assertions.
    We can run these assertions using 'runAssertJudges'. -}
sectionLinkedAssert :: Section c -> [C.Assert c]
sectionLinkedAssert Section{ sectionRelmap = ms, sectionAssert = ass }
    = map linker ass where
      linker = C.assertMap $ C.relmapLinker ms



-- ----------------------  Constructors

{-| Section that has no contents. -}
makeEmptySection :: C.RelmapCons c -> Section c
makeEmptySection = Section Nothing [] [] [] [] [] ""

{-| Section that has no contents. -}
emptySection :: Section c
emptySection = makeEmptySection $ C.relmapCons []

{-| Section that has only here data. -}
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
