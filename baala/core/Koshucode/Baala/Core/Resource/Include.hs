{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

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


type Include c = C.ClauseHead -> [S.Token] -> C.ClauseBody -> B.Ab (C.Resource c)

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
       res <- B.foldM (resIncludeBody cd) base $ reverse cs
       Right res { C.resSelect = C.datasetSelect $ C.dataset $ C.resJudge res }

resIncludeBody :: forall c. (D.CContent c) =>
    FilePath -> C.Resource c -> C.Clause -> C.AbResource c
resIncludeBody cd base (C.Clause h b) =
    let sec    = C.clauseSecNo h
        toks   = B.takeFirst $ B.clauseTokens $ C.clauseSource h
        call f = Msg.abClause toks $ do
                   res <- f h toks b
                   Right $ res { C.resLastSecNo = sec }
    in case b of
         C.CJudge   _ _ _    -> call judge
         C.CAssert  _ _ _    -> call assert
         C.CRelmap  _ _      -> call relmap
         C.CSlot    _ _      -> call slot
         C.CInput   _        -> feat (call input)  C.featInputClause  Msg.disabledInputClause  
         C.COutput  _        -> feat (call output) C.featOutputClause Msg.disabledOutputClause
         C.COption  _        -> call option
         C.CEcho    _        -> call echo
         C.CLicense _        -> call license
         C.CUnknown (Left a) -> Left a
    where
      f << y  = y : f base

      feature = C.resFeature base
      feat e f msg | f feature = e
                   | otherwise = msg

      judge :: Include c
      judge _ _ (C.CJudge q p toks) =
          do trees <- S.ttrees toks
             js    <- D.treesToJudge calc q p trees
             Right $ base { C.resJudge = C.resJudge << js }

      calc :: D.ContentCalc c
      calc = calcContG $ C.resGlobal base

      assert :: Include c
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh }
                          src (C.CAssert typ pat toks) =
          do optPara <- C.ttreePara2 toks
             let ass   = C.Assert sec typ pat src optPara Nothing []
                 ass'  = S.Short (B.codePtList $ head src) sh ass
             Right $ base { C.resAssert = C.resAssert << ass' }

      relmap :: Include c
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right $ base { C.resLexmap = C.resLexmap  << ((sec, n), lt) }

      slot :: Include c
      slot _ _ (C.CSlot n toks) =
          do trees <- S.ttrees toks
             Right base { C.resSlot = C.resSlot << (n, trees) }

      option :: Include c
      option _ _ (C.COption toks) =
          do opt <- C.optionParse calc toks (C.resOption base)
             Right $ base { C.resOption = opt }

      input :: Include c
      input _ _ (C.CInput toks) =
          do io <- ioPoint toks
             checkIOPoint $ C.resQueueTodo io base

      output :: Include c
      output _ _ (C.COutput toks) =
          do io <- ioPoint toks
             checkIOPoint $ base { C.resOutput = C.inputPoint io }

      ioPoint :: [S.Token] -> B.Ab C.InputPoint
      ioPoint = C.ttreePara2 B.>=> paraToIOPoint cd

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res = let ins = C.resInput  res
                             out = C.resOutput res
                         in if out `elem` ins
                            then Msg.sameIOPoints out
                            else Right res

      echo :: Include c
      echo _ _ (C.CEcho clause) =
          Right $ base { C.resEcho = C.resEcho << B.clauseLines clause }

      license :: Include c
      license _ _ (C.CLicense line) =
          Right $ base { C.resLicense = C.resLicense << (C.clauseSecNo h, line) }

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

