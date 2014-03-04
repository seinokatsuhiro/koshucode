{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

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
  Section (..),
  AbbrAsserts,
  assertViolated,
  assertNormal,

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


-- ----------------------  Abbr

assertViolated :: B.Map (AbbrAsserts c)
assertViolated = B.shortMap $ filter C.isViolateAssert

assertNormal :: B.Map (AbbrAsserts c)
assertNormal = B.shortMap $ filter $ not . C.isViolateAssert

type AbbrAsserts c = [B.Short [C.Assert c]]


-- ----------------------  Section

data Section c = Section {
      sectionName     :: Maybe String           -- ^ Section name
    , sectionImport   :: [Section c]            -- ^ Importing section
    , sectionExport   :: [String]               -- ^ Exporting relmap names
    , sectionShort    :: [[B.Named String]]     -- ^ Prefix for short signs
    , sectionAssert   :: AbbrAsserts c          -- ^ Assertions of relmaps
    , sectionRelmap   :: [B.Named (C.Relmap c)] -- ^ Relmaps and its name
    , sectionJudge    :: [B.Judge c]            -- ^ Affirmative or denial judgements
    , sectionViolate  :: [B.Judge c]            -- ^ Violated judgements, i.e., result of @|=V@
    , sectionResource :: B.Resource             -- ^ Resource name
    , sectionCons     :: C.RelmapCons c         -- ^ Relmap constructor for this section
    } deriving (Show)

instance (Ord c, B.Pretty c) => B.Pretty (Section c) where
    doc sec = dSection where
        dSection = B.docv [dRelmap, dAssert, dJudge]
        dRelmap  = B.docv $ map docRelmap $ sectionRelmap sec
        docRelmap (n,m) = B.docZero (n ++ " :") B.<+> B.doc m B.$$ B.doc ""
        dJudge   = B.docv $ sectionJudge sec
        dAssert  = B.docv $ concatMap B.shortBody $ sectionAssert sec

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
makeEmptySection = Section Nothing [] [] [] [] [] [] [] (B.ResourceText "")

{-| Section that has no contents. -}
emptySection :: Section c
emptySection = makeEmptySection $ C.relmapCons C.global

-- {-| Section that has only judgements. -}
-- dataSection :: [B.Judge c] -> Section c
-- dataSection js = emptySection { sectionJudge = js }



-- ----------------------  Full construction

-- | Second step of constructing 'Section'.
consSection
    :: forall c. (C.CContent c)
    => C.RelmapConsFull c    -- ^ Relmap full constructor
    -> B.Resource            -- ^ Resource name
    -> [B.Short [C.Clause]]  -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)      -- ^ Result section
consSection consFull resource xss =
    do sects <- mapM (consSectionEach consFull resource) xss
       Right $ M.mconcat sects

consSectionEach
    :: forall c. (C.CContent c)
    => C.RelmapConsFull c   -- ^ Relmap full constructor
    -> B.Resource           -- ^ Resource name
    -> B.Short [C.Clause]   -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)     -- ^ Result section
consSectionEach consFull resource (B.Short shorts xs) =
    do _        <-  mapMFor unk    isCUnknown
       _        <-  mapMFor unres  isCUnres
       imports  <-  mapMFor impt   isCImport
       judges   <-  mapMFor judge  isCJudge 
       relmaps  <-  mapMFor relmap isCRelmap
       asserts  <-  mapMFor assert isCAssert

       Right $ emptySection
           { sectionName      =  section xs
           , sectionImport    =  imports
           , sectionExport    =  mapFor expt isCExport
           , sectionShort     =  mapFor short isCShort
           , sectionAssert    =  [B.Short shorts asserts]
           , sectionRelmap    =  relmaps
           , sectionJudge     =  judges
           , sectionResource  =  resource }
    where
      mapFor  f p = pass     f  `map`  filter (p . C.clauseBody) xs
      mapMFor f p = pass (ab f) `mapM` filter (p . C.clauseBody) xs
      pass f (C.Clause src body) = f (B.front $ B.clauseTokens src) body
      consSec = consSection consFull (B.ResourceText "")
      ab f toks body = B.abortable "clause" toks $ f toks body

      -- todo: multiple section name
      section ((C.Clause _ (C.CSection n)) : _) = n
      section (_ : xs2) = section xs2
      section [] = Nothing

      expt  _ (C.CExport n) = n

      short :: [B.Token] -> C.ClauseBody -> [B.Named String]
      short  _ (C.CShort   p) = p

      impt _ (C.CImport _ (Nothing)) = Right emptySection
      impt _ (C.CImport _ (Just _))  = consSec []

      judge :: [B.Token] -> C.ClauseBody -> B.Ab (B.Judge c)
      judge _ (C.CJudge q pat xs2) =
          case C.litJudge q pat (B.tokenTrees xs2) of
            Right j -> Right j
            Left  a -> abort a

      relmap :: [B.Token] -> C.ClauseBody -> B.Ab (B.Named (C.Relmap c))
      relmap _ (C.CRelmap name lx) =
          case consFull lx of
            Right full -> Right (name, full)
            Left a     -> abort a

      assert :: [B.Token] -> C.ClauseBody -> B.Ab (C.Assert c)
      assert toks (C.CAssert typ pat opt lx) =
          case consFull lx of
            Right full -> Right $ C.Assert typ pat opt full toks
            Left a     -> abort a

      unk   _ (C.CUnknown) = Left $ B.AbortSyntax [] B.ASUnkClause
      unres _ (C.CUnres _) = Left $ B.AbortSyntax [] B.ASUnresToken
      abort (B.AbortSyntax _ a) = Left $ B.AbortSyntax [] a
      abort a = Left a

isCImport, isCExport, isCShort,
  isCRelmap, isCAssert, isCJudge,
  isCUnknown, isCUnres :: C.ClauseBody -> Bool

isCImport (C.CImport _ _)       = True
isCImport _                     = False

isCExport (C.CExport _)         = True
isCExport _                     = False

isCShort (C.CShort _)           = True
isCShort _                      = False

isCRelmap (C.CRelmap _ _)       = True
isCRelmap _                     = False

isCAssert (C.CAssert _ _ _ _)   = True
isCAssert _                     = False

isCJudge (C.CJudge _ _ _)       = True
isCJudge _                      = False

isCUnknown (C.CUnknown)         = True
isCUnknown _                    = False

isCUnres (C.CUnres _)           = True
isCUnres _                      = False



-- ----------------------
-- $Process
--  
--  Section is constructed using following steps.
--  
--  [1. @??? -> String@]
--      Get string from something.
--  
--  [2. @String -> \[String\]@]
--      Split string into lines.
--  
--  [3. @\[String\] -> \[Token\]@]
--      Split line-breaked strings into tokens.
--
--  [4. @\[Token\] -> \[\[Token\]\]@]
--      Collect tokens for clauses.
--
--  [5. @\[\[Token\]\] -> \[Clause\]@]
--      Classify tokens.
--
--  [6. @\[Clause\] -> Section a@]
--      Make section from list of clauses.
