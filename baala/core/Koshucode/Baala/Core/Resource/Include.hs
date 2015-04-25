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


type Include c = C.ClauseHead -> [B.Token] -> C.ClauseBody -> B.Ab (C.Resource c)

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

resIncludeBody :: forall c. (C.CContent c) =>
    C.Resource c -> B.Ab C.Clause -> C.AbResource c
resIncludeBody res abcl =
    do C.Clause h b <- abcl
       let sec   = C.clauseSecNo h
           toks  = B.front $ B.clauseTokens $ C.clauseSource h
           call f = Msg.abClause toks $ do
                      res' <- f h toks b
                      Right $ res' { C.resLastSecNo = sec }
       case b of
         C.CJudge   _ _ _  -> call judge
         C.CAssert  _ _ _  -> call assert
         C.CRelmap  _ _    -> call relmap
         C.CSlot    _ _    -> call slot
         C.CInput   _      -> call input
         C.COutput  _      -> call output
         C.COption  _      -> call option
         C.CEcho    _      -> call echo
    where
      f << y  = y : f res
      f <: t  = case f res of (todo1, todo2, done) -> (t : todo1, todo2, done)

      judge :: Include c
      judge _ _ (C.CJudge q p toks) =
          do trees <- B.ttrees toks
             js    <- C.treesToJudge calc q p trees
             Right $ res { C.resJudge = C.resJudge << js }

      calc :: C.ContentCalc c
      calc = calcContG $ C.resGlobal res

      assert :: Include c
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh }
                          src (C.CAssert typ pat toks) =
          do optPara <- C.ttreePara2 toks
             let ass   = C.Assert sec typ pat src optPara Nothing []
                 ass'  = B.Short (B.codePtList $ head src) sh ass
             Right $ res { C.resAssert = C.resAssert << ass' }

      relmap :: Include c
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right $ res { C.resLexmap = C.resLexmap  << ((sec, n), lt) }

      slot :: Include c
      slot _ _ (C.CSlot n toks) =
          do trees <- B.ttrees toks
             Right res { C.resSlot = C.resSlot << (n, trees) }

      option :: Include c
      option _ _ (C.COption toks) =
          do opt <- C.optionParse calc toks (C.resOption res)
             Right $ res { C.resOption = opt }

      input :: Include c
      input _ _ (C.CInput toks) =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resInputStack = C.resInputStack <: io }

      output :: Include c
      output _ _ (C.COutput toks) =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resOutput = io }

      ioPoint :: [B.Token] -> B.Ab B.IOPoint
      ioPoint = C.ttreePara2 B.>=> paraToIOPoint

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res' = let ins = C.resInput  res'
                              out = C.resOutput res'
                          in if out `elem` ins
                             then Msg.sameIOPoints out
                             else Right res'

      echo :: Include c
      echo _ _ (C.CEcho clause) =
          Right $ res { C.resEcho = C.resEcho << B.clauseLines clause }


coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (C.CContent c) => C.Global c -> C.ContentCalc c
calcContG = C.calcContent . C.globalCopset

paraToIOPoint :: C.TTreePara -> B.Ab B.IOPoint
paraToIOPoint = B.paraSelect unmatch ps where
    ps = [ (just1, B.paraType `B.paraJust` 1)
         , (stdin, B.paraType `B.paraReq` ["stdin"]) ]

    just1 p = do arg <- B.paraGetFst p
                 case arg of
                   B.TextLeaf _ _ path -> Right $ B.ioPointFrom path
                   _ -> Msg.adlib "input not text"

    stdin p = do args <- B.paraGet p "stdin"
                 case args of
                   [] -> Right B.IOPointStdin
                   _  -> Msg.adlib "input no args"

    unmatch = Msg.adlib "input unknown"


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

