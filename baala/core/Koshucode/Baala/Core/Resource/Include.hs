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
       let sec   = C.clauseSecNo h
           toks  = B.front $ B.clauseTokens $ C.clauseSource h
           f `to` res2 = do x <- Msg.abClause toks $ f h toks b
                            Right $ (res2 x) { C.resLastSecNo = sec }
       case b of
         C.CJudge _ _ _    -> judge  `to` \x -> res { C.resJudge   = C.resJudge   << x }
         C.CAssert _ _ _ _ -> assert `to` \x -> res { C.resAssert  = C.resAssert  << x }
         C.CRelmap _ _     -> relmap `to` \x -> res { C.resLexmap  = C.resLexmap  << x }
         C.CSlot _ _       -> slot   `to` \x -> res { C.resSlot    = C.resSlot    << x }
         C.CInclude _      -> inc    `to` \x -> res { C.resArticle = C.resArticle <: x }
    where
      f << y  = y : f res
      f <: t  = case f res of (todo1, todo2, done) -> (t : todo1, todo2, done)

      judge :: Clab (B.Judge c)
      judge _ _ (C.CJudge q p toks) =
          C.treesToJudge calc q p =<< B.tokenTrees toks

      calc = calcContG $ C.resGlobal res

      assert :: Clab (C.ShortAssert' h c)
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh } src (C.CAssert typ pat opt toks) =
          do optPara    <- C.ttreePara1 opt
             rmapTrees  <- B.tokenTrees toks
             Right $ B.Short (B.codePtList $ head src) sh
                       $ C.Assert sec typ pat optPara src rmapTrees Nothing []

      relmap :: Clab C.LexmapClause
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right ((sec, n), lt)

      slot :: Clab B.NamedTrees
      slot _ _ (C.CSlot n toks) =
          do trees <- B.tokenTrees toks
             Right (n, trees)

      inc :: Clab B.CodeName
      inc _ _ (C.CInclude toks) =
          paraToCodeName =<< C.ttreePara1 toks

coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (C.CContent c) => C.Global c -> C.CalcContent c
calcContG = C.calcContent . C.globalCopset

paraToCodeName :: C.TTreePara -> B.Ab B.CodeName
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

