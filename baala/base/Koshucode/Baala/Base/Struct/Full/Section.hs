{-# OPTIONS_GHC -Wall #-}

-- | Data structure for relational calculations.
--   There are three types of section:
--   (1) /editing sections/ that output judges and read other sections,
--   (2) /library sections/ that make relmaps reusable,
--   (3) /data sections/ that provide data.

module Koshucode.Baala.Base.Struct.Full.Section
(
  -- * Process
  -- $Process

  -- * Section
  Section (..)
, sectionLinkedAssert

  -- * Constructors
, makeEmptySection
, emptySection
, dataSection
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.RelmapCons

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
    doc = docSection

docSection :: (Ord v, Pretty v) => Section v -> Doc
docSection md = dSection where
    dSection = docv [dRelmap, dAssert, dJudge]
    dRelmap = docv $ map docRelmap $ sectionRelmap md
    docRelmap (n,m) = zeroWidthText (n ++ " :") <+> doc m $$ text ""
    dJudge  = docv $ sectionJudge md
    dAssert = docv $ sectionAssert md



-- ----------------------  Linking relmaps

-- | Select assertions like 'sectionAssert'.
--   It returns relmap-liked assertions.
--   We can run these assertions using 'runAssertJudges'.
sectionLinkedAssert :: Section v -> [Assert v]
sectionLinkedAssert md = map a $ sectionAssert md where
    rs'    = linkRelmap $ sectionRelmap md
    linker = makeLinker rs'
    a (Assert q s r) = Assert q s $ linker r

linkRelmap :: [Named (Relmap v)] -> [Named (Relmap v)]
linkRelmap rs = rs' where
    rs'     = map f rs
    linker  = makeLinker rs'
    f (n,r) = (n, linker r)

makeLinker :: [Named (Relmap v)] -> Relmap v -> Relmap v
makeLinker rs' = link where
    link (RelmapAppend r1 r2)  = RelmapAppend (link r1) (link r2)
    link (RelmapCalc h n f rs) = RelmapCalc h n f $ map link rs
    link r@(RelmapName _ n)    = case lookup n rs' of
                                   Just r' -> r'
                                   Nothing -> r
    link r                     = r



-- ----------------------  Constructor

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

