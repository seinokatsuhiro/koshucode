{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Resource as bundle of relational expressions.

module Koshucode.Baala.Core.Resource.Resource
  (
    -- * Data type
    Resource (..), coxBuildG,
    addMessage, addMessages,

    -- * Constructors
    resEmpty, resInclude,
  
    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, RopUse, ShortAssert,

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
      resGlobal    :: Global c           -- ^ Global parameter
    , resImport    :: [Resource c]       -- ^ Importing resources
    , resExport    :: [String]           -- ^ Exporting names
    , resSlot      :: [B.NamedTrees]     -- ^ Global slots
    , resRelmap    :: [C.RelmapSource]   -- ^ Source of relmaps
    , resAssert    :: [ShortAssert c]    -- ^ Assertions of relmaps
    , resJudge     :: [B.Judge c]        -- ^ Affirmative or denial judgements
    , resSource    :: B.Source           -- ^ Source name
    , resMessage   :: [String]           -- ^ Collection of messages
    , resLastSecNo :: C.SecNo            -- ^ Last section number
    , resSelect    :: C.RelSelect c
    }

instance Show (Resource c) where
    show Resource { resSource = src }
        = "Resources (" ++ show src ++ ")"

instance B.SelectRel Resource where
    selectRel Resource { resSelect = sel } = sel

instance C.GetGlobal Resource where
    getGlobal Resource { resGlobal = g } = g

addMessage :: String -> B.Map (Resource c)
addMessage msg res = res { resMessage = msg : resMessage res }

addMessages :: [String] -> B.Map (Resource c)
addMessages msg res = res { resMessage = msg ++ resMessage res }

-- | Resource that has no contents.
resEmpty :: Resource c
resEmpty = Resource
           { resGlobal     = C.global
           , resImport     = []
           , resExport     = []
           , resSlot       = []
           , resRelmap     = []
           , resAssert     = []
           , resJudge      = []
           , resSource     = B.sourceZero
           , resMessage    = []
           , resLastSecNo  = 0
           , resSelect     = \_ _ -> B.reldee
           }


-- ----------------------  Construction

-- | Include source code into resource.
resInclude :: forall c. (C.CContent c)
    => Resource c          -- ^ Base resource
    -> B.Source            -- ^ Source name
    -> String              -- ^ Source code
    -> B.Ab (Resource c)   -- ^ Included resource
resInclude res src code =
    do ls <- B.tokenLines src code
       let include  = resIncludeEach src
           sec      = resLastSecNo res + 1
           shorts   = C.consClause sec ls
       B.foldM include res shorts

type Cl   a  = C.SecNo -> [B.Token] -> C.ClauseBody -> a
type Clab a  = Cl (B.Ab a)

resIncludeEach :: forall c. (C.CContent c) =>
    B.Source -> Resource c -> C.ShortClause -> B.Ab (Resource c)
resIncludeEach source res (B.Short pt shorts xs) =
    do _        <- forM isCUnknown unk
       _        <- forM isCUnres   unres
       imports  <- forM isCImport  impt
       judges   <- forM isCJudge   judge
       slots    <- forM isCSlot    slot
       relmaps  <- forM isCRelmap  relmap
       asserts  <- forM isCAssert  assert

       checkShort shorts

       Right $ up $ res
           { resImport     = resImport  << imports
           , resExport     = resExport  << for isCExport expt
           , resSlot       = resSlot    << slots
           , resRelmap     = resRelmap  << relmaps
           , resAssert     = resAssert  << [B.Short pt shorts asserts]
           , resJudge      = resJudge   << judges
           , resSource     = source
           , resLastSecNo  = lastSecNo xs }
    where
      f << ys    = ys ++ f res
      for  isX f = pass     f  `map`  filter (isX . C.clauseBody) xs
      forM isX f = pass (ab f) `mapM` filter (isX . C.clauseBody) xs

      pass f (C.Clause src sec body) = f sec (B.front $ B.clauseTokens src) body
      ab f sec toks body = Msg.abClause toks $ f sec toks body

      lastSecNo :: [C.Clause] -> Int
      lastSecNo []   = 0
      lastSecNo xs2  = C.clauseSecNo $ last xs2

      up res2@Resource { resJudge = js }
          = res2 { resSelect = C.datasetSelect $ C.dataset js }

      -- Type of clauses

      expt :: Cl String
      expt _ _ (C.CExport n) = n

      impt :: Clab (Resource c)
      impt _ _ (C.CImport _ (Nothing)) = Right resEmpty
      impt _ _ (C.CImport _ (Just _))  = resInclude res B.sourceZero []

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

      calc = calcContG $ resGlobal res

      assert :: Clab (C.Assert' h c)
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

calcContG :: (C.CContent c) => Global c -> C.CalcContent c
calcContG = C.calcContent . C.globalCopset

coxBuildG :: (C.CContent c) => Global c -> B.TTreeToAb (C.Cox c)
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


-- ----------------------  Hook

type Assert c           = C.Assert'          Resource c
type ConsRelmap c       = C.ConsRelmap'      Resource c
type Global c           = C.Global'          Resource c
type Relmap c           = C.Relmap'          Resource c
type RelkitHook c       = C.RelkitHook'      Resource c
type RelmapLinkTable c  = C.RelmapLinkTable' Resource c
type Rop c              = C.Rop'             Resource c
type RopCons c          = C.RopCons'         Resource c
type RopUse c           = C.RopUse'          Resource c
type ShortAssert c      = C.ShortAssert'     Resource c


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
