{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Resource as bundle of relational expressions.

module Koshucode.Baala.Core.Resource.Include
  ( resInclude, coxBuildG,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Clause    as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Data.Message            as Msg
import qualified Koshucode.Baala.Core.Relmap.Message     as Msg
import qualified Koshucode.Baala.Core.Resource.Message   as Msg


-- | Include source code into resource.
resInclude :: forall c. (D.CContent c)
    => [S.Token]        -- ^ Additional terms
    -> FilePath         -- ^ Context directory
    -> C.Resource c     -- ^ Base resource
    -> B.NIOPoint       -- ^ Input point
    -> B.Bz             -- ^ Source code
    -> C.AbResource c   -- ^ Included resource
resInclude resAbout cd base nio code =
    do ls <- S.tokenLinesBz nio code
       let sec  = C.resLastSecNo base + 1
           cs   = C.consClause resAbout sec ls
           cs'  = reverse cs
           sec' | null cs'  = sec
                | otherwise = C.clauseSecNo $ C.clauseHead $ head cs'
       res <- B.foldM (resIncludeBody cd) base cs'
       Right res { C.resLastSecNo = sec'
                 , C.resSelect    = C.datasetSelect $ C.dataset $ C.resJudge res }

resIncludeBody :: forall c. (D.CContent c) =>
    FilePath -> C.Resource c -> C.Clause -> C.AbResource c
resIncludeBody cd res (C.Clause h@C.ClauseHead{ C.clauseSecNo = sec, C.clauseShort = sh } b) =
    case b of
      C.CJudge   q p toks     -> ab $ judge q p toks
      C.CAssert  typ cl toks  -> ab $ assert typ cl toks
      C.CRelmap  n toks       -> ab $ relmap n toks
      C.CSlot    n toks       -> ab $ slot n toks
      C.CInput   toks         -> feat (ab $ input toks)  C.featInputClause  Msg.disabledInputClause  
      C.COutput  toks         -> feat (ab $ output toks) C.featOutputClause Msg.disabledOutputClause
      C.COption  toks         -> ab $ option toks
      C.CEcho    clause       -> ab $ echo clause
      C.CLicense line         -> ab $ license line
      C.CUnknown (Left a)     -> Left a
      _                       -> B.bug "resIncludeBody"
    where

      -- ----------------------  Utility

      src :: [S.Token]
      src = B.takeFirst $ B.clauseTokens $ C.clauseSource h

      ab = Msg.abClause src

      feature = C.resFeature res
      feat e f msg | f feature = e
                   | otherwise = msg

      calc :: D.ContentCalc c
      calc = calcContG $ C.resGlobal res

      f << y  = y : f res

      -- ----------------------  Clause

      judge q p toks =
          do trees <- S.ttrees toks
             js    <- D.treesToJudge calc q p trees
             Right $ res { C.resJudge = C.resJudge << js }

      assert typ cl toks =
          do optPara <- C.ttreePara2 toks
             let ass   = C.Assert sec typ cl src optPara Nothing []
                 ass'  = S.Short (B.codePtList $ head src) sh ass
             Right $ res { C.resAssert = C.resAssert << ass' }

      relmap n toks =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right $ res { C.resLexmap = C.resLexmap  << ((sec, n), lt) }

      slot n toks =
          do trees <- S.ttrees toks
             Right res { C.resSlot = C.resSlot << (n, trees) }

      option toks =
          do opt <- C.optionParse calc toks $ C.resOption res
             Right $ res { C.resOption = opt }

      input toks =
          do io <- ioPoint toks
             checkIOPoint $ C.resQueueTodo io res

      output toks =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resOutput = C.inputPoint io }

      ioPoint :: [S.Token] -> B.Ab C.InputPoint
      ioPoint = C.ttreePara2 B.>=> paraToIOPoint cd

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res' = let ins = C.resInput  res'
                              out = C.resOutput res'
                         in if out `elem` ins
                            then Msg.sameIOPoints out
                            else Right res'

      echo clause =
          Right $ res { C.resEcho = C.resEcho << B.clauseLines clause }

      license line =
          Right $ res { C.resLicense = C.resLicense << (C.clauseSecNo h, line) }

coxBuildG :: (D.CContent c) => C.Global c -> S.TTreeToAb (D.Cox c)
coxBuildG g = D.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (D.CContent c) => C.Global c -> D.ContentCalc c
calcContG = D.calcContent . C.globalCopset

paraToIOPoint :: FilePath -> C.TTreePara -> B.Ab C.InputPoint
paraToIOPoint cd = S.paraSelect unmatch ps where
    ps = [ (pJust1, just1)
         , (pStdin, stdin) ]

    pJust1 = S.paraSpec $ S.paraJust 1 . S.paraOpt ["about"]
    pStdin = S.paraSpec $ S.paraReq ["stdin"]

    just1 :: C.TTreePara -> B.Ab C.InputPoint
    just1 p = do arg   <- S.paraGetFst p
                 about <- S.paraGetOpt [] p "about"
                 case arg of
                   S.TextLeaf _ _ path -> Right $ C.InputPoint (B.ioPointFrom cd path) about
                   _ -> Msg.adlib "input not text"

    stdin :: C.TTreePara -> B.Ab C.InputPoint
    stdin p = do args <- S.paraGet p "stdin"
                 case args of
                   [] -> Right $ C.InputPoint B.IOPointStdin []
                   _  -> Msg.adlib "input no args"

    unmatch = Msg.adlib "input unknown"

