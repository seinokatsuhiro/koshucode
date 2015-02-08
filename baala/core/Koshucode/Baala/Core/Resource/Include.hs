{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
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
       let sec  = C.resLastSecNo res + 1
           cs   = C.consClause sec ls
       res2 <- B.foldM resIncludeBody res $ reverse cs
       Right res2 { C.resSelect = C.datasetSelect $ C.dataset $ C.resJudge res2 }

type Cl   a  = C.ClauseHead -> [B.Token] -> C.ClauseBody -> a
type Clab a  = Cl (B.Ab a)

resIncludeBody :: forall c. (C.CContent c) =>
    C.Resource c -> B.Ab C.Clause -> C.AbResource c
resIncludeBody res abcl =
    do C.Clause h b <- abcl
       let secNo = C.clauseSecNo h
           toks  = B.front $ B.clauseTokens $ C.clauseSource h
           f `call` res2 = do x <- Msg.abClause toks $ f h toks b
                              Right $ (res2 x) { C.resLastSecNo = secNo }
       case True of
         _ | isCJudge   b -> judge  `call` \x -> res { C.resJudge   = C.resJudge   << x }
           | isCAssert  b -> assert `call` \x -> res { C.resAssert  = C.resAssert  << x }
           | isCRelmap  b -> relmap `call` \x -> res { C.resRelmap  = C.resRelmap  << x }
           | isCSlot    b -> slot   `call` \x -> res { C.resSlot    = C.resSlot    << x }
           | isCInclude b -> inc    `call` \x -> res { C.resArticle = C.resArticle <: x }
    where
      f << y  = y : f res
      f <: t  = case f res of (todo1, todo2, done) -> (t : todo1, todo2, done)

      judge :: Clab (B.Judge c)
      judge _ _ (C.CJudge q p toks) =
          C.treesToJudge calc q p =<< B.tokenTrees toks

      calc = calcContG $ C.resGlobal res

      assert :: Clab (C.ShortAssert' h c)
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh } src (C.CAssert typ pat opt toks) =
          do optPara    <- C.tokenPara opt
             rmapTrees  <- B.tokenTrees toks
             Right $ B.Short (B.codePtList $ head src) sh
                       $ C.Assert sec typ pat optPara src rmapTrees Nothing []

      relmap :: Clab C.RelmapSource
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          case B.splitTokensBy (== "---") toks of
            Left  _         -> ntrees2 sec n toks []
            Right (r, _, e) -> ntrees2 sec n r e

      ntrees2 :: C.SecNo -> String -> [B.Token] -> [B.Token] -> B.Ab C.RelmapSource
      ntrees2 sec n toks1 toks2 =
          do form    <- B.tokenTrees  toks1
             trees2  <- B.tokenTrees  toks2
             edit    <- C.consAttrmap trees2
             Right ((sec, n), (form, edit))

      slot :: Clab B.NamedTrees
      slot _ _ (C.CSlot n toks) = ntrees (n, toks)

      ntrees :: (String, [B.Token]) -> B.Ab (String, [B.TTree])
      ntrees (n, toks) = do trees <- B.tokenTrees toks
                            Right (n, trees)

      inc :: Clab B.CodeName
      inc _ _ (C.CInclude toks) =
          C.tokenPara toks >>= paraToCodeName

coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (C.CContent c) => C.Global c -> C.CalcContent c
calcContG = C.calcContent . C.globalCopset

paraToCodeName :: C.TokenPara -> B.Ab B.CodeName
paraToCodeName = B.paraSelect unmatch ps where
    ps = [ (just1, B.paraType `B.paraJust` 1)
         , (stdin, B.paraType `B.paraReq` ["-stdin"]) ]

    just1 p = do arg <- B.paraGetFst p
                 case arg of
                   B.TextLeaf _ _ path -> Right $ B.codeNameFrom path
                   _ -> Msg.adlib "include not text"

    stdin p = do args <- B.paraGet p "-stdin"
                 case args of
                   [] -> Right B.CodeStdin
                   _  -> Msg.adlib "include no args"

    unmatch = Msg.adlib "include unknown"


-- ----------------------  Clause type

isCInclude, isCSlot, isCRelmap, isCAssert, isCJudge
    :: C.ClauseBody -> Bool

isCInclude (C.CInclude _)      = True
isCInclude _                   = False

isCSlot (C.CSlot _ _)          = True
isCSlot _                      = False

isCRelmap (C.CRelmap _ _)      = True
isCRelmap _                    = False

isCAssert (C.CAssert _ _ _ _)  = True
isCAssert _                    = False

isCJudge (C.CJudge _ _ _)      = True
isCJudge _                     = False



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

