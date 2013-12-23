{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

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
  consSection,
) where

import qualified Data.Monoid                          as M
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Clause  as C

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
    s1 { sectionName    = Nothing
       , sectionImport  = []
       , sectionExport  = union sectionExport
       , sectionAssert  = union sectionAssert
       , sectionRelmap  = union sectionRelmap
       , sectionJudge   = union sectionJudge
       , sectionViolate = union sectionViolate
       } where union f  = f s1 ++ f s2

{-| Make empty section that has a given constructor. -}
makeEmptySection :: C.RelmapCons c -> Section c
makeEmptySection = Section Nothing [] [] [] [] [] [] [] ""

{-| Section that has no contents. -}
emptySection :: Section c
emptySection = makeEmptySection $ C.relmapCons []

-- {-| Section that has only judgements. -}
-- dataSection :: [B.Judge c] -> Section c
-- dataSection js = emptySection { sectionJudge = js }



-- ----------------------  Full construction

{-| Second step of constructing 'Section'. -}
consSection
    :: (C.CContent c)
    => C.RelmapFullCons c      -- ^ Relmap full constructor
    -> String                  -- ^ Resource name
    -> [C.Clause]              -- ^ Output of 'C.consClause'
    -> B.AbortOr (Section c)   -- ^ Result section
consSection full resource xs =
    do _        <-  mapMFor unk    isCUnknown
       _        <-  mapMFor unres  isCUnres
       imports  <-  mapMFor impt   isCImport
       judges   <-  mapMFor judge  isCJudge 
       relmaps  <-  mapMFor relmap isCRelmap
       asserts  <-  mapMFor assert isCAssert

       Right $ emptySection
           { sectionName      =  section xs
           , sectionImport    =  imports
           , sectionExport    =  mapFor expt  isCExport
           , sectionShort     =  mapFor short isCShort
           , sectionAssert    =  asserts
           , sectionRelmap    =  relmaps
           , sectionJudge     =  judges
           , sectionResource  =  resource }
    where
      mapFor  f p = pass f `map`  filter (p . C.clauseBody) xs
      mapMFor f p = pass f `mapM` filter (p . C.clauseBody) xs
      pass f (C.Clause src body) = f (B.clauseLines src) body
      consSec = consSection full ""

      -- todo: multiple section name
      section ((C.Clause _ (C.CSection n)) : _) = n
      section (_ : xs2) = section xs2
      section [] = Nothing

      expt  _ (C.CExport n) = n
      short _ (C.CShort p)  = p

      impt _ (C.CImport _ (Nothing)) = Right emptySection
      impt _ (C.CImport _ (Just e))  = consSec [e]

      judge ls (C.CJudge q p xs2) =
          case C.litJudge q p (B.tokenTrees xs2) of
            Right j -> Right j
            Left  a -> abort ls a

      relmap _ (C.CRelmap n r) =
          case full r of
            Right r'  -> Right (n, r')
            Left a    -> abort [] a

      assert ls (C.CAssert t p opt r) =
          case full r of
            Right r'  -> Right $ C.Assert t p opt r' ls
            Left a    -> abort [] a

      unk   ls (C.CUnknown) = abort [] $ B.AbortSyntax ls B.ASUnkClause
      unres ls (C.CUnres _) = abort [] $ B.AbortSyntax ls B.ASUnresToken
      abort ls (B.AbortSyntax _ a) = Left (B.AbortSyntax ls a, [])
      abort ls a = Left (a, ls)

isCImport, isCExport, isCShort,
  isCRelmap, isCAssert, isCJudge,
  isCUnknown, isCUnres :: C.ClauseBody -> Bool

isCImport (C.CImport _ _)      = True
isCImport _                    = False

isCExport (C.CExport _)        = True
isCExport _                    = False

isCShort (C.CShort _)          = True
isCShort _                     = False

isCRelmap (C.CRelmap _ _)      = True
isCRelmap _                    = False

isCAssert (C.CAssert _ _ _ _)  = True
isCAssert _                    = False

isCJudge (C.CJudge _ _ _)      = True
isCJudge _                     = False

isCUnknown (C.CUnknown)        = True
isCUnknown _                   = False

isCUnres (C.CUnres _)          = True
isCUnres _                     = False



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

