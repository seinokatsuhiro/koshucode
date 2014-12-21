{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Resource as bundle of relational expressions.

module Koshucode.Baala.Core.Resource.Resource
  (
    -- * Data type
    Resource (..),
    coxBuildG,
    addMessage,
    addMessages,
  
    -- * Constructors
    emptyResource,
    rootResource,
    consResource,
    concatResource,
  
    -- * Process
    -- $Process
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Content          as C
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap           as C
import qualified Koshucode.Baala.Core.Assert           as C
import qualified Koshucode.Baala.Core.Resource.Clause  as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- ----------------------  Data type

data Resource c = Resource {
      resGlobal    :: C.Global c          -- ^ Global parameter
    , resImport    :: [Resource c]        -- ^ Importing resources
    , resExport    :: [String]            -- ^ Exporting names
    , resSlot      :: [B.NamedTrees]      -- ^ Global slots
    , resRelmap    :: [C.RelmapSource]    -- ^ Source of relmaps
    , resAssert    :: [C.ShortAssert c]   -- ^ Assertions of relmaps
    , resJudge     :: [B.Judge c]         -- ^ Affirmative or denial judgements
    , resSource    :: B.Source            -- ^ Source name
    , resMessage   :: [String]            -- ^ Collection of messages
    , resLastSecNo :: C.SecNo             -- ^ Last section number
    } deriving (Show)

addMessage :: String -> B.Map (Resource c)
addMessage msg res = res { resMessage = msg : resMessage res }

addMessages :: [String] -> B.Map (Resource c)
addMessages msg res = res { resMessage = msg ++ resMessage res }

-- | Resource that has no contents.
emptyResource :: Resource c
emptyResource = Resource C.global [] [] [] [] [] [] B.sourceZero [] 0

-- | Construct root resource from global parameter.
rootResource :: C.Global c -> Resource c
rootResource g = emptyResource { resGlobal = g }

-- | Concatenate resources into united resource.
concatResource :: Resource c -> [Resource c] -> Resource c
concatResource root rs =
    root { resImport      = []
         , resExport      = cat resExport
         , resSlot        = cat resSlot
         , resAssert      = cat resAssert
         , resRelmap      = cat resRelmap
         , resJudge       = cat resJudge
         , resLastSecNo   = maximum $ map resLastSecNo rs
         } where cat = (`concatMap` rs)


-- ----------------------  Construction

-- | Second step of constructing 'Resource'.
consResource
    :: forall c. (C.CContent c)
    => Resource c          -- ^ Root resource
    -> B.Source            -- ^ Source name
    -> [C.ShortClause]     -- ^ Output of 'C.consClause'
    -> B.Ab (Resource c)   -- ^ Result resource
consResource root source xss =
    do rs <- consResourceEach root source `mapM` xss
       Right $ concatResource root rs

consResourceEach :: forall c. (C.CContent c) =>
    Resource c -> B.Source -> C.ShortClause -> B.Ab (Resource c)
consResourceEach root source (B.Short pt shorts xs) =
    do _        <- forM isCUnknown unk
       _        <- forM isCUnres   unres
       imports  <- forM isCImport  impt
       judges   <- forM isCJudge   judge
       slots    <- forM isCSlot    slot
       relmaps  <- forM isCRelmap  relmap
       asserts  <- forM isCAssert  assert

       checkShort shorts

       Right $ root
           { resImport     = imports
           , resExport     = for isCExport expt
           , resSlot       = slots
           , resRelmap     = relmaps
           , resAssert     = [B.Short pt shorts asserts]
           , resJudge      = judges
           , resSource     = source
           , resLastSecNo  = lastSecNo xs }
    where
      for  isX f = pass     f  `map`  filter (isX . C.clauseBody) xs
      forM isX f = pass (ab f) `mapM` filter (isX . C.clauseBody) xs

      pass f (C.Clause src sec body) = f sec (B.front $ B.clauseTokens src) body
      ab f sec toks body = Msg.abClause toks $ f sec toks body

      lastSecNo :: [C.Clause] -> Int
      lastSecNo []   = 0
      lastSecNo xs2  = C.clauseSecNo $ last xs2

      expt :: Cl String
      expt _ _ (C.CExport n) = n

      impt :: Clab (Resource c)
      impt _ _ (C.CImport _ (Nothing)) = Right emptyResource
      impt _ _ (C.CImport _ (Just _))  = consResource root B.sourceZero []

      slot :: Clab B.NamedTrees
      slot _ _ (C.CSlot n toks) = ntrees (n, toks)

      ntrees :: (String, [B.Token]) -> B.Ab (String, [B.TTree])
      ntrees (n, toks) = do trees <- B.tokenTrees toks
                            Right (n, trees)

      relmap :: Clab C.RelmapSource
      relmap sec _ (C.CRelmap n toks) =
          case B.splitTokensBy (== "---") toks of
            Left  _         -> ntrees2 sec n toks []
            Right (r, _, e) -> ntrees2 sec n r e

      ntrees2 :: C.SecNo -> String -> [B.Token] -> [B.Token] -> B.Ab C.RelmapSource
      ntrees2 sec n toks1 toks2 =
          do form    <- B.tokenTrees  toks1
             trees2  <- B.tokenTrees  toks2
             edit    <- C.consAttrmap trees2
             Right ((sec, n), (form, edit))

      judge :: Clab (B.Judge c)
      judge _ _ (C.CJudge q p toks) =
          C.treesToJudge calc q p =<< B.tokenTrees toks

      calc = calcContG $ resGlobal root

      assert :: Clab (C.Assert c)
      assert sec src (C.CAssert typ pat opt toks) =
          do optTrees   <- B.tokenTrees opt
             rmapTrees  <- B.tokenTrees toks
             let optAssc = C.hyphenAssc optTrees
             Right $ C.Assert sec typ pat optAssc src rmapTrees Nothing []

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

      unk   _ _ (C.CUnknown)  = Msg.unkClause
      unres _ _ (C.CUnres _)  = Msg.unresPrefix

type Cl   a  = C.SecNo -> [B.Token] -> C.ClauseBody -> a
type Clab a  = Cl (B.Ab a)

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
--  Resource is constructed using following steps.
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
--  [6. @\[Clause\] -> Resource a@]
--      Make resource from list of clauses.
