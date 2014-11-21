{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Section as bundle of relational expressions.

module Koshucode.Baala.Core.Section.Section
  (
    -- * Data type
    Section (..),
    coxBuildG,
    addMessage,
    addMessages,
  
    -- * Constructors
    emptySection,
    rootSection,
    consSection,
    concatSection,
  
    -- * Process
    -- $Process
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Msg



-- ----------------------  Data type

data Section c = Section {
      secName     :: Maybe String        -- ^ Section name
    , secGlobal   :: C.Global c          -- ^ Global parameter
    , secImport   :: [Section c]         -- ^ Importing section
    , secExport   :: [String]            -- ^ Exporting relmap names
    , secSlot     :: [B.NamedTrees]      -- ^ Global slots
    , secRelmap   :: [C.RelmapSource]    -- ^ Source of relmaps
    , secAssert   :: [C.ShortAssert c]   -- ^ Assertions of relmaps
    , secJudge    :: [B.Judge c]         -- ^ Affirmative or denial judgements
    , secSource   :: B.Source            -- ^ Source name
    , secCons     :: C.RelmapCons c      -- ^ Relmap constructor for this section
    , secMessage  :: [String]            -- ^ Collection of messages
    } deriving (Show)

addMessage :: String -> B.Map (Section c)
addMessage msg sec = sec { secMessage = msg : secMessage sec }

addMessages :: [String] -> B.Map (Section c)
addMessages msg sec = sec { secMessage = msg ++ secMessage sec }

-- | Section that has no contents.
emptySection :: Section c
emptySection = Section Nothing C.global [] [] [] [] [] [] B.sourceZero cons [] where
    cons = C.relmapCons C.global

-- | Construct root section from global parameter.
rootSection :: C.Global c -> Section c
rootSection g = emptySection { secGlobal = g
                             , secCons   = C.relmapCons g }

-- | Concatenate sections into united section.
concatSection :: Section c -> [Section c] -> Section c
concatSection root ss =
    root { secName    =  Nothing
         , secImport  =  []
         , secExport  =  c secExport
         , secSlot    =  c secSlot
         , secAssert  =  c secAssert
         , secRelmap  =  c secRelmap
         , secJudge   =  c secJudge
         } where c = (`concatMap` ss)



-- ----------------------  Construction

-- | Second step of constructing 'Section'.
consSection
    :: forall c. (C.CContent c)
    => Section c          -- ^ Root section
    -> B.Source           -- ^ Source name
    -> [C.ShortClause]    -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)   -- ^ Result section
consSection root source xss =
    do sects <- consSectionEach root source `mapM` xss
       Right $ concatSection root sects

consSectionEach :: forall c. (C.CContent c) =>
    Section c -> B.Source -> C.ShortClause -> B.Ab (Section c)
consSectionEach root source (B.Short pt shorts xs) =
    do _        <-  forM isCUnknown unk
       _        <-  forM isCUnres   unres
       imports  <-  forM isCImport  impt
       judges   <-  forM isCJudge   judge
       slots    <-  forM isCSlot    slot
       relmaps  <-  forM isCRelmap  relmap
       asserts  <-  forM isCAssert  assert

       checkShort shorts

       Right $ root
           { secName      =  name xs
           , secImport    =  imports
           , secExport    =  for isCExport expt
           , secSlot      =  slots
           , secRelmap    =  relmaps
           , secAssert    =  [B.Short pt shorts asserts]
           , secJudge     =  judges
           , secSource    =  source }
    where
      for  isX f = pass     f  `map`  filter (isX . C.clauseBody) xs
      forM isX f = pass (ab f) `mapM` filter (isX . C.clauseBody) xs

      pass f (C.Clause src body) = f (B.front $ B.clauseTokens src) body
      consSec = consSection root (B.sourceZero)
      ab f toks body = Msg.abClause toks $ f toks body

      -- todo: multiple section name
      name ((C.Clause _ (C.CSection n)) : _) = n
      name (_ : xs2) = name xs2
      name [] = Nothing

      expt :: Cl String
      expt _ (C.CExport n) = n

      impt :: Clab (Section c)
      impt _ (C.CImport _ (Nothing)) = Right emptySection
      impt _ (C.CImport _ (Just _))  = consSec []

      slot :: Clab B.NamedTrees
      slot _ (C.CSlot n toks) = ntrees (n, toks)

      ntrees :: (String, [B.Token]) -> B.Ab (String, [B.TTree])
      ntrees (n, toks) = do trees <- B.tokenTrees toks
                            Right (n, trees)

      relmap :: Clab C.RelmapSource
      relmap _ (C.CRelmap n toks) =
          case B.splitTokensBy (== "---") toks of
            Left  _         -> ntrees2 n toks []
            Right (r, _, e) -> ntrees2 n r e

      ntrees2 :: String -> [B.Token] -> [B.Token] -> B.Ab (String, ([B.TTree], C.Roamap))
      ntrees2 n toks1 toks2 =
          do trees1 <- B.tokenTrees toks1
             trees2 <- B.tokenTrees toks2
             roamap <- C.roamapCons trees2
             Right (n, (trees1, roamap))

      judge :: Clab (B.Judge c)
      judge _ (C.CJudge q p toks) =
          C.treesToJudge calc q p =<< B.tokenTrees toks

      calc = calcContG $ secGlobal root

      assert :: Clab (C.Assert c)
      assert src (C.CAssert typ pat opt toks) =
          do optTrees   <- B.tokenTrees opt
             rmapTrees  <- B.tokenTrees toks
             let optAssc = C.hyphenAssc optTrees
             Right $ C.Assert typ pat optAssc src rmapTrees Nothing []

      checkShort :: [B.ShortDef] -> B.Ab ()
      checkShort sh =
          Msg.abShort pt $ do
            let (ss, rs) = unzip sh
                prefix   = B.duplicates ss
                replace  = B.duplicates rs
                invalid  = B.omit B.isShortPrefix ss
            B.unless (null prefix)  $ Msg.dupPrefix prefix
            B.unless (null replace) $ Msg.dupReplacement replace
            B.unless (null invalid) $ Msg.invalidPrefix invalid
            Right ()

      unk   _ (C.CUnknown) = Msg.unkClause
      unres _ (C.CUnres _) = Msg.unresPrefix

type Cl   a = [B.Token] -> C.ClauseBody -> a
type Clab a = Cl (B.Ab a)

calcContG :: (C.CContent c) => C.Global c -> C.CalcContent c
calcContG = C.calcContent . C.globalCopset

coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)


-- ----------------------  Clause type

isCImport, isCExport,
  isCSlot, isCRelmap, isCAssert, isCJudge,
  isCUnknown, isCUnres :: C.ClauseBody -> Bool

isCImport (C.CImport _ _)      = True
isCImport _                    = False

isCExport (C.CExport _)        = True
isCExport _                    = False

isCSlot (C.CSlot _ _)          = True
isCSlot _                      = False

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
