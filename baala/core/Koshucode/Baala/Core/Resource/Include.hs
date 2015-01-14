{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Resource as bundle of relational expressions.

module Koshucode.Baala.Core.Resource.Include
  ( resInclude, coxBuildG,
    -- * Process
    -- $Process
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Core.Content            as C
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Clause    as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Message            as Msg


-- | Include source code into resource.
resInclude :: forall c. (C.CContent c)
    => C.Resource c     -- ^ Base resource
    -> B.CodePiece      -- ^ Source name
    -> String           -- ^ Source code
    -> C.AbResource c   -- ^ Included resource
resInclude res src code =
    do ls <- B.tokenLines src code
       let sec      = C.resLastSecNo res + 1
           shorts   = C.consClause sec ls
       B.foldM resIncludeBody res shorts

type Cl   a  = C.SecNo -> [B.Token] -> C.ClauseBody -> a
type Clab a  = Cl (B.Ab a)

resIncludeBody :: forall c. (C.CContent c) =>
    C.Resource c -> C.ShortClause -> C.AbResource c
resIncludeBody res (B.Short pt shorts xs) =
    do _        <- forM isCUnknown unk
       _        <- forM isCUnres   unres
       incs     <- forM isCInclude inc
       judges   <- forM isCJudge   judge
       slots    <- forM isCSlot    slot
       relmaps  <- forM isCRelmap  relmap
       asserts  <- forM isCAssert  assert

       checkShort shorts

       Right $ up $ res
           { C.resExport     = C.resExport   << for isCExport expt
           , C.resSlot       = C.resSlot     << slots
           , C.resRelmap     = C.resRelmap   << relmaps
           , C.resAssert     = C.resAssert   << [B.Short pt shorts asserts]
           , C.resJudge      = C.resJudge    << judges
           , C.resArticle    = C.resArticle  <: reverse incs
           , C.resLastSecNo  = lastSecNo xs }
    where
      f << ys   = ys ++ f res
      f <: t    = case f res of (todo1, todo2, done) -> (t ++ todo1, todo2, done)

      for  isX f = pass     f  `map`  filter (isX . C.clauseBody) xs
      forM isX f = pass (ab f) `mapM` filter (isX . C.clauseBody) xs

      pass f (C.Clause src sec body) = f sec (B.front $ B.clauseTokens src) body
      ab f sec toks body = Msg.abClause toks $ f sec toks body

      lastSecNo :: [C.Clause] -> Int
      lastSecNo []   = 0
      lastSecNo xs2  = C.clauseSecNo $ last xs2

      up res2@C.Resource { C.resJudge = js }
          = res2 { C.resSelect = C.datasetSelect $ C.dataset js }

      -- Type of clauses

      expt :: Cl String
      expt _ _ (C.CExport n) = n

      inc :: Clab B.CodeName
      inc _ _ (C.CInclude _ (Just path))  = Right $ B.codeNameFrom path
      inc _ _ _                           = Msg.adlib "include"

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

      calc = calcContG $ C.resGlobal res

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

coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (C.CContent c) => C.Global c -> C.CalcContent c
calcContG = C.calcContent . C.globalCopset


-- ----------------------  Clause type

isCInclude, isCExport,
  isCSlot, isCRelmap, isCAssert, isCJudge,
  isCUnknown, isCUnres :: C.ClauseBody -> Bool

isCInclude (C.CInclude _ _)    = True
isCInclude _                   = False

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
--  1. Get string from something.
--  
--  2. Split string into lines.
--  
--  3. Split line-breaked strings into tokens.
--
--  4. Collect tokens for clauses.
--
--  5. Classify tokens.
--
--  6. Construct resource from list of clauses.

