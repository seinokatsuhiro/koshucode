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
  ShortAsserts,
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
import qualified Koshucode.Baala.Core.Message         as Message


-- ----------------------  Short

assertViolated :: B.Map (ShortAsserts c)
assertViolated = B.shortMap $ filter C.isViolateAssert

assertNormal :: B.Map (ShortAsserts c)
assertNormal = B.shortMap $ filter $ not . C.isViolateAssert

type ShortAsserts c = [B.Short [C.Assert c]]


-- ----------------------  Section

data Section c = Section {
      secName     :: Maybe String        -- ^ Section name
    , secImport   :: [Section c]         -- ^ Importing section
    , secExport   :: [String]            -- ^ Exporting relmap names
    , secShort    :: [[B.Named String]]  -- ^ Prefix for short signs
    , secSlot     :: [B.NamedTrees]      -- ^ Global slot
    , secAssert   :: ShortAsserts c      -- ^ Assertions of relmaps
    , secTokmap   :: [B.NamedTrees]
    , secJudge    :: [B.Judge c]         -- ^ Affirmative or denial judgements
    , secViolate  :: [B.Judge c]         -- ^ Violated judgements, i.e., result of @|=V@
    , secResource :: B.Resource          -- ^ Resource name
    , secCons     :: C.RelmapCons c      -- ^ Relmap constructor for this section
    } deriving (Show)

instance (Ord c, B.Pretty c) => B.Pretty (Section c) where
    doc sec = dSection where
        dSection = B.docv [dAssert, dJudge]
        dJudge   = B.docv $ secJudge sec
        dAssert  = B.docv $ concatMap B.shortBody $ secAssert sec

instance M.Monoid (Section c) where
    mempty  = emptySection
    mappend = secUnion

secUnion :: Section c -> Section c -> Section c
secUnion s1 s2 =
    s1 { secName    = Nothing
       , secImport  = []
       , secExport  = union secExport
       , secSlot    = union secSlot
       , secAssert  = union secAssert
       , secTokmap  = union secTokmap
       , secJudge   = union secJudge
       , secViolate = union secViolate
       } where union f = f s1 ++ f s2

{-| Make empty section that has a given constructor. -}
makeEmptySection :: C.RelmapCons c -> Section c
makeEmptySection = Section Nothing [] [] [] [] [] [] [] []
                   (B.ResourceText "")

{-| Section that has no contents. -}
emptySection :: Section c
emptySection = makeEmptySection $ C.relmapCons C.global

-- {-| Section that has only judgements. -}
-- dataSection :: [B.Judge c] -> Section c
-- dataSection js = emptySection { secJudge = js }



-- ----------------------  Full construction

-- | Second step of constructing 'Section'.
consSection
    :: forall c. (C.CContent c)
    => Section c
    -> B.Resource            -- ^ Resource name
    -> [B.Short [C.Clause]]  -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)      -- ^ Result section
consSection root resource xss =
    do sects <- mapM (consSectionEach root resource) xss
       Right $ M.mconcat sects

consSectionEach
    :: forall c. (C.CContent c)
    => Section c
    -> B.Resource           -- ^ Resource name
    -> B.Short [C.Clause]   -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)     -- ^ Result section
consSectionEach root resource (B.Short shorts xs) =
    do _        <-  mapMFor unk     isCUnknown
       _        <-  mapMFor unres   isCUnres
       imports  <-  mapMFor impt    isCImport
       judges   <-  mapMFor judge   isCJudge 
       asserts  <-  mapMFor assert  isCAssert

       Right $ root
           { secName      =  section xs
           , secImport    =  imports
           , secExport    =  mapFor expt isCExport
           , secShort     =  mapFor short isCShort
           , secSlot      =  mapFor slot isCSlot
           , secTokmap    =  mapFor tokmap isCTokmap
           , secAssert    =  [B.Short shorts asserts]
           , secJudge     =  judges
           , secResource  =  resource }
    where
      mapFor  f p = pass     f  `map`  filter (p . C.clauseBody) xs
      mapMFor f p = pass (ab f) `mapM` filter (p . C.clauseBody) xs
      pass f (C.Clause src body) = f (B.front $ B.clauseTokens src) body
      consSec = consSection root (B.ResourceText "")
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

      slot :: [B.Token] -> C.ClauseBody -> B.NamedTrees
      slot _ (C.CSlot name trees) = (name, trees)

      tokmap :: [B.Token] -> C.ClauseBody -> B.NamedTrees
      tokmap _ (C.CTokmap name trees) = (name, trees)

      assert :: [B.Token] -> C.ClauseBody -> B.Ab (C.Assert c)
      assert toks (C.CAssert typ pat opt trees) =
          do lx <- lexmap trees
             Right $ C.Assert typ pat opt lx Nothing [] toks

      lexmap = C.consLexmap $ secCons root

      unk   _ (C.CUnknown) = Message.unkClause
      unres _ (C.CUnres _) = Message.unresPrefix
      abort a = Left a


-- ----------------------  Clause type

isCImport, isCExport, isCShort,
  isCSlot, isCTokmap, isCAssert, isCJudge,
  isCUnknown, isCUnres :: C.ClauseBody -> Bool

isCImport (C.CImport _ _)      = True
isCImport _                    = False

isCExport (C.CExport _)        = True
isCExport _                    = False

isCShort (C.CShort _)          = True
isCShort _                     = False

isCSlot (C.CSlot _ _)          = True
isCSlot _                      = False

isCTokmap (C.CTokmap _ _)      = True
isCTokmap _                    = False

isCAssert (C.CAssert _ _ _ _)  = True
isCAssert _                    = False

isCJudge (C.CJudge _ _ _)      = True
isCJudge _                     = False

isCUnknown (C.CUnknown)        = True
isCUnknown _                   = False

isCUnres (C.CUnres _)          = True
isCUnres _                     = False



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
