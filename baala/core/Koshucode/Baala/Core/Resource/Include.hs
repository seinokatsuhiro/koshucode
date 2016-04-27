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
import qualified Koshucode.Baala.Syntax                  as D
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Clause    as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Data.Message            as Msg
import qualified Koshucode.Baala.Core.Resource.Message   as Msg


type Include c = C.ClauseHead -> [D.Token] -> C.ClauseBody -> B.Ab (C.Resource c)

-- | Include source code into resource.
resInclude :: forall c. (D.CContent c)
    => [D.Token]        -- ^ Additional terms
    -> FilePath         -- ^ Context directory
    -> C.Resource c     -- ^ Base resource
    -> B.CodePiece      -- ^ Source name
    -> String           -- ^ Source code
    -> C.AbResource c   -- ^ Included resource
resInclude add cd res src code =
    do ls <- D.tokenLines src code
       let sec  = C.resLastSecNo res + 1
           cs   = C.consClause add sec ls
       res2 <- B.foldM (resIncludeBody cd) res $ reverse cs
       Right res2 { C.resSelect = C.datasetSelect $ C.dataset $ C.resJudge res2 }

resIncludeBody :: forall c. (D.CContent c) =>
    FilePath -> C.Resource c -> B.Ab C.Clause -> C.AbResource c
resIncludeBody cd res abcl =
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
         C.CLicense _      -> call license
    where
      f << y  = y : f res
      f <: t  = case f res of (todo1, todo2, done) -> (t : todo1, todo2, done)

      judge :: Include c
      judge _ _ (C.CJudge q p toks) =
          do trees <- D.ttrees toks
             js    <- D.treesToJudge calc q p trees
             Right $ res { C.resJudge = C.resJudge << js }

      calc :: D.ContentCalc c
      calc = calcContG $ C.resGlobal res

      assert :: Include c
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh }
                          src (C.CAssert typ pat toks) =
          do optPara <- C.ttreePara2 toks
             let ass   = C.Assert sec typ pat src optPara Nothing []
                 ass'  = D.Short (B.codePtList $ head src) sh ass
             Right $ res { C.resAssert = C.resAssert << ass' }

      relmap :: Include c
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right $ res { C.resLexmap = C.resLexmap  << ((sec, n), lt) }

      slot :: Include c
      slot _ _ (C.CSlot n toks) =
          do trees <- D.ttrees toks
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
             checkIOPoint $ res { C.resOutput = C.inputPoint io }

      ioPoint :: [D.Token] -> B.Ab C.InputPoint
      ioPoint = C.ttreePara2 B.>=> paraToIOPoint cd

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res' = let ins = C.resInput  res'
                              out = C.resOutput res'
                          in if out `elem` ins
                             then Msg.sameIOPoints out
                             else Right res'

      echo :: Include c
      echo _ _ (C.CEcho clause) =
          Right $ res { C.resEcho = C.resEcho << B.clauseLines clause }

      license :: Include c
      license h _ (C.CLicense line) =
          Right $ res { C.resLicense = C.resLicense << (C.clauseSecNo h, line) }

coxBuildG :: (D.CContent c) => C.Global c -> D.TTreeToAb (D.Cox c)
coxBuildG g = D.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (D.CContent c) => C.Global c -> D.ContentCalc c
calcContG = D.calcContent . C.globalCopset

paraToIOPoint :: FilePath -> C.TTreePara -> B.Ab C.InputPoint
paraToIOPoint cd = D.paraSelect unmatch ps where
    ps = [ (just1, D.paraType `D.paraJust` 1 `D.paraOpt` ["about"])
         , (stdin, D.paraType `D.paraReq` ["stdin"]) ]

    just1 :: C.TTreePara -> B.Ab C.InputPoint
    just1 p = do arg   <- D.paraGetFst p
                 about <- D.paraGetOpt [] p "about"
                 case arg of
                   D.TextLeaf _ _ path -> Right $ C.InputPoint (B.ioPointFrom cd path) about
                   _ -> Msg.adlib "input not text"

    stdin :: C.TTreePara -> B.Ab C.InputPoint
    stdin p = do args <- D.paraGet p "stdin"
                 case args of
                   [] -> Right $ C.InputPoint B.IOPointStdin []
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

