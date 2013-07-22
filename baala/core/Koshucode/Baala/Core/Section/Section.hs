{-# OPTIONS_GHC -Wall #-}

-- | Data structure for relational calculations.
--   There are three types of section:
--   (1) /editing sections/ that output judges and read other sections,
--   (2) /library sections/ that make relmaps reusable,
--   (3) /data sections/ that provide data.

module Koshucode.Baala.Core.Section.Section
(
  -- * Process
  -- $Process

  -- * Section
  Section (..)

  -- * Selectors
, sectionLinkedAssert

  -- * Constructors
, makeEmptySection
, emptySection
, dataSection
) where

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Core.Relmap

data Section v = Section {
      sectionName   :: Maybe String       -- ^ Section name
    , sectionImport :: [Section v]        -- ^ Importing section
    , sectionExport :: [String]           -- ^ Exporting relmap names
    , sectionAssert :: [Assert v]         -- ^ Assertions of relmaps
    , sectionRelmap :: [Named (Relmap v)] -- ^ Relmaps and its name
    , sectionJudge  :: [Judge v]          -- ^ Here data
    , sectionCons   :: RelmapCons v       -- ^ Readers and writers for this section
    } deriving (Show)

instance (Ord v, Pretty v) => Pretty (Section v) where
    doc sec = dSection where
        dSection = docv [dRelmap, dAssert, dJudge]
        dRelmap  = docv $ map docRelmap $ sectionRelmap sec
        docRelmap (n,m) = zeroWidthText (n ++ " :") <+> doc m $$ text ""
        dJudge   = docv $ sectionJudge sec
        dAssert  = docv $ sectionAssert sec



-- ----------------------  Selectors

{-| Select assertions like 'sectionAssert'.
    It returns relmap-liked assertions.
    We can run these assertions using 'runAssertJudges'. -}
sectionLinkedAssert :: Section v -> [Assert v]
sectionLinkedAssert Section{ sectionRelmap = ms, sectionAssert = ass }
    = map linker ass where
      linker = assertMap $ relmapLinker ms



-- ----------------------  Constructors

-- | Section that has no contents.
makeEmptySection :: RelmapCons v -> Section v
makeEmptySection = Section Nothing [] [] [] [] []

-- | Section that has no contents.
emptySection :: Section v
emptySection = makeEmptySection $ relmapCons []

-- | Section that has only here data.
dataSection :: [Judge v] -> Section v
dataSection js = emptySection { sectionJudge = js }



-- ----------------------
-- $Process
--
-- Section is constructed using following steps.
-- 
-- [1. @??? -> String@]
--     Get string from something.
-- 
-- [2. @String -> \[String\]@]
--     Split string into lines.
-- 
-- [3. @\[String\] -> \[Token\]@]
--     Split line-breaked strings into tokens.
--
-- [4. @\[Token\] -> \[\[Token\]\]@]
--     Collect tokens for clauses.
--
-- [5. @\[\[Token\]\] -> \[Clause\]@]
--     Classify tokens.
--
-- [6. @\[Clause\] -> Section a@]
--     Make section from list of clauses.

