{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Section as bundle of relational expressions.

module Koshucode.Baala.Core.Section.Section
(
  -- * Data type
  Section (..),
  calcTreeUsing,
  coxTreeUsing,

  -- * Constructors
  emptySection,
  consSection,
  rootSection,
  addMessage,
  addMessages,

  -- * Process
  -- $Process
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Message



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
    , secResource :: B.Resource          -- ^ Resource name
    , secCons     :: C.RelmapCons c      -- ^ Relmap constructor for this section
    , secMessage  :: [String]            -- ^ Collection of messages
    } deriving (Show)

instance B.Monoid (Section c) where
    mempty  = emptySection
    mappend = appendSection

appendSection :: Section c -> Section c -> Section c
appendSection s1 s2 =
    s1 { secName    = Nothing
       , secImport  = []
       , secExport  = union secExport
       , secSlot    = union secSlot
       , secAssert  = union secAssert
       , secRelmap  = union secRelmap
       , secJudge   = union secJudge
       } where union f = f s1 ++ f s2

-- | Section that has no contents.
emptySection :: Section c
emptySection = Section Nothing C.global [] [] [] [] [] [] res cons [] where
    res  = B.ResourceText ""
    cons = C.relmapCons C.global

rootSection :: C.Global c -> Section c
rootSection g = emptySection { secGlobal = g
                             , secCons   = C.relmapCons g }

addMessage :: String -> B.Map (Section c)
addMessage msg sec = sec { secMessage = msg : secMessage sec }

addMessages :: [String] -> B.Map (Section c)
addMessages msg sec = sec { secMessage = msg ++ secMessage sec }



-- ----------------------  Construction

-- | Second step of constructing 'Section'.
consSection
    :: forall c. (C.CContent c)
    => Section c          -- ^ Root section
    -> B.Resource         -- ^ Resource name
    -> [C.ShortClause]    -- ^ Output of 'C.consClause'
    -> B.Ab (Section c)   -- ^ Result section
consSection root resource xss =
    do sects <- consSectionEach root resource `mapM` xss
       Right $ B.mconcat sects

consSectionEach :: forall c. (C.CContent c) =>
    Section c -> B.Resource -> C.ShortClause -> B.Ab (Section c)
consSectionEach root resource (B.Short pt shorts xs) =
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
           , secResource  =  resource }
    where
      for  isX f = pass     f  `map`  filter (isX . C.clauseBody) xs
      forM isX f = pass (ab f) `mapM` filter (isX . C.clauseBody) xs

      pass f (C.Clause src body) = f (B.front $ B.clauseTokens src) body
      consSec = consSection root (B.ResourceText "")
      ab f toks body = Message.abClause toks $ f toks body

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

      ntrees :: (String, [B.Token]) -> B.Ab (String, [B.TokenTree])
      ntrees (n, toks) = do trees <- B.tokenTrees toks
                            Right (n, trees)

      relmap :: Clab C.RelmapSource
      relmap _ (C.CRelmap n toks) =
          case B.splitTokensBy (== "---") toks of
            Left  _         -> ntrees2 n toks []
            Right (r, _, e) -> ntrees2 n r e

      ntrees2 :: String -> [B.Token] -> [B.Token] -> B.Ab (String, ([B.TokenTree], C.Roamap))
      ntrees2 n toks1 toks2 =
          do trees1 <- B.tokenTrees toks1
             trees2 <- B.tokenTrees toks2
             roamap <- C.roamapCons trees2
             Right (n, (trees1, roamap))

      judge :: Clab (B.Judge c)
      judge _ (C.CJudge q p toks) =
          C.getJudge calc q p =<< B.tokenTrees toks

      calc = secCalc root

      assert :: Clab (C.Assert c)
      assert src (C.CAssert typ pat opt toks) =
          do optTrees   <- B.tokenTrees opt
             rmapTrees  <- B.tokenTrees toks
             let optAssc = C.hyphenAssc optTrees
             Right $ C.Assert typ pat optAssc src rmapTrees Nothing []

      checkShort :: [B.ShortDef] -> B.Ab ()
      checkShort sh =
          Message.abShort pt $ do
            let (ss, rs) = unzip sh
                prefix   = B.duplicates ss
                replace  = B.duplicates rs
                invalid  = B.omit B.isShortPrefix ss
            B.unless (null prefix)  $ Message.dupPrefix prefix
            B.unless (null replace) $ Message.dupReplacement replace
            B.unless (null invalid) $ Message.invalidPrefix invalid
            Right ()

      unk   _ (C.CUnknown) = Message.unkClause
      unres _ (C.CUnres _) = Message.unresPrefix

type Cl   a = [B.Token] -> C.ClauseBody -> a
type Clab a = Cl (B.Ab a)

secCalc :: (C.CContent c) => Section c -> C.CalcContent c
secCalc = calcTreeUsing . secGlobal

calcTreeUsing :: (C.CContent c) => C.Global c -> C.CalcContent c
calcTreeUsing g = calc where
    base       =  C.globalFunction g
    syntax     =  C.globalSyntax   g
    copset     =  C.globalCopset g
    calc tree  =  do alpha <- C.coxBuild calc copset syntax tree
                     C.coxRunCox (base, []) B.mempty [] alpha

coxTreeUsing :: (C.CContent c) => C.Global c -> B.TokenTreeToAb (C.Cox c)
coxTreeUsing g = C.coxBuild undefined (C.globalCopset g) $ C.globalSyntax g


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
