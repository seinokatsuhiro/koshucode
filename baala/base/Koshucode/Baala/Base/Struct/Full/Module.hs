{-# OPTIONS_GHC -Wall #-}

-- | Data structure for relational calculations.
--   There are three types of module:
--   (1) /editing modules/ that output judges and read other modules,
--   (2) /library modules/ that make relmaps reusable,
--   (3) /data modules/ that provide data.

module Koshucode.Baala.Base.Struct.Full.Module
(
  -- * Process
  -- $Process

  -- * Section
  Module (..)
, moduleLinkedAssert

  -- * Constructors
, makeEmptyModule
, emptyModule
, dataModule
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.HalfRelmap

data Module v = Module {
      moduleName   :: Maybe String       -- ^ Module name
    , moduleImport :: [Module v]         -- ^ Importing module
    , moduleExport :: [String]           -- ^ Exporting relmap names
    , moduleAssert :: [Assert v]         -- ^ Assertions of relmaps
    , moduleRelmap :: [Named (Relmap v)] -- ^ Relmaps and its name
    , moduleJudge  :: [Judge v]          -- ^ Here data
    , moduleCons   :: ConsRelmap v       -- ^ Readers and writers for this module
    } deriving (Show)

instance (Ord v, Pretty v) => Pretty (Module v) where
    doc = docModule

docModule :: (Ord v, Pretty v) => Module v -> Doc
docModule md = dModule where
    dModule = docv [dRelmap, dAssert, dJudge]
    dRelmap = docv $ map docRelmap $ moduleRelmap md
    docRelmap (n,m) = zeroWidthText (n ++ " :") <+> doc m $$ text ""
    dJudge  = docv $ moduleJudge md
    dAssert = docv $ moduleAssert md



-- ----------------------  Linking relmaps

-- | Select assertions like 'moduleAssert'.
--   It returns relmap-liked assertions.
--   We can run these assertions using 'runAssertJudges'.
moduleLinkedAssert :: Module v -> [Assert v]
moduleLinkedAssert md = map a $ moduleAssert md where
    rs'    = linkRelmap $ moduleRelmap md
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

-- | Module that has no contents.
makeEmptyModule :: ConsRelmap v -> Module v
makeEmptyModule = Module Nothing [] [] [] [] []

-- | Module that has no contents.
emptyModule :: Module v
emptyModule = makeEmptyModule $ makeConsRelmap []

-- | Module that has only here data.
dataModule :: [Judge v] -> Module v
dataModule js = emptyModule { moduleJudge = js }



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

