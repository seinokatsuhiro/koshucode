{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Resource as bundle of relational expressions.

module Koshucode.Baala.Core.Resource.Include
  ( resInclude,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as T
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Assert             as C
import qualified Koshucode.Baala.Core.Resource.Clause    as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Syntax.Pattern          as P
import qualified Koshucode.Baala.Data.Message            as Msg
import qualified Koshucode.Baala.Core.Relmap.Message     as Msg
import qualified Koshucode.Baala.Core.Resource.Message   as Msg

-- | Include source code into resource.
resInclude :: (S.TextualTermName t, S.ToTrees S.Chars [S.TToken t], D.CContent c)
    => [S.TToken t]     -- ^ Additional terms
    -> FilePath         -- ^ Context directory
    -> C.Resource c     -- ^ Base resource
    -> B.IxIOPoint      -- ^ Input point
    -> B.Bytes          -- ^ Source code
    -> C.AbResource c   -- ^ Included resource
resInclude resAbout cd res xio code =
    do let ls   = S.tokenLines xio code
           sec  = C.resLastSecNo res + 1
           cs   = C.consClause resAbout sec ls
       (cache, js, cs2) <- createJudges res cs
       let ds   = D.dataset js
           cs2' = reverse cs2
           sec' | null cs   = sec
                | otherwise = C.clauseSecNo $ C.clauseHead $ last cs
       res' <- B.foldM (resIncludeNonJudge cd) res cs2'
       Right res' { C.resLastSecNo = sec'
                  , C.resCacheT    = cache
                  , C.resJudge     = js
                  , C.resDataset   = ds }

createJudges :: (O.Textual t, S.ToTrees S.Chars [S.TToken t], D.CContent c)
    => C.Resource c -> [C.Clause t] -> B.Ab (D.CacheT S.Chars, [T.Judge c], [C.Clause t])
createJudges res = loop $ C.resCacheT res where
    loop cc ((C.Clause h (C.CJudge q cl toks)) : cs) =
        Msg.abClause h $ do
           trees          <- S.toTrees toks
           (cc1, judge)   <- D.treesJudge cc q cl trees
           (cc2, js, cs') <- loop cc1 cs
           Right (cc2, judge : js, cs')

    loop cc (c : cs)  = do (cc', js, cs') <- loop cc cs
                           Right (cc', js, c : cs')
    loop cc []        = Right (cc, C.resJudge res, [])

resIncludeNonJudge :: forall t c. (O.Textual t, D.CContent c) =>
    FilePath -> C.Resource c -> C.Clause t -> C.AbResource c
resIncludeNonJudge cd res (C.Clause h@C.ClauseHead{ C.clauseSecNo = sec, C.clauseShort = sh } b) =
    case b of
      C.CAssert  q cl toks  -> ab $ assert q cl $ chars toks
      C.CRelmap  n toks     -> ab $ relmap n $ chars toks
      C.CSlot    n toks     -> ab $ slot n toks
      C.CInput   toks       -> feat (ab $ input  $ chars toks) C.featInputClause  Msg.disabledInputClause
      C.COutput  toks       -> feat (ab $ output $ chars toks) C.featOutputClause Msg.disabledOutputClause
      C.COption  toks       -> ab $ option $ chars toks
      C.CEcho    clause     -> ab $ echo clause
      C.CLicense line       -> ab $ license line
      C.CUnknown (Left a)   -> Left a
      _                     -> B.bug "resIncludeNonJudge"
    where

      -- ----------------------  Utility

      chars = (S.tChars O.<$$>)

      short (t1, t2) = (O.tString t1, O.tString t2)

      src :: [S.TToken S.Chars]
      src = B.takeFirst $ chars $ B.clauseTokens $ C.clauseSource h

      ab = Msg.abClause [h]

      feature = C.resFeature res
      feat e f msg | f feature = e
                   | otherwise = msg

      calc :: D.CalcContent S.Chars c
      calc = calcContG $ C.resGlobal res

      f << y  = y : f res

      -- ----------------------  Clause

      assert typ cl toks =
          do optPara <- C.ttreePara2 toks
             let ass   = C.Assert sec typ cl src optPara Nothing []
                 ass'  = S.Short (B.getCPs src) (short <$> sh) ass
             Right $ res { C.resAssert = C.resAssert << ass' }

      relmap n toks =
          do lt <- C.consLexmapTrees O.# C.ttreePara2 toks
             Right $ res { C.resLexmap = C.resLexmap  << ((sec, n), lt) }

      slot n toks =
          do trees <- S.toTrees $ chars toks
             Right res { C.resSlot = C.resSlot << (O.tString n, trees) }

      option toks =
          do opt <- C.optionParse calc toks $ C.resOption res
             Right $ res { C.resOption = opt }

      input toks =
          do io <- ioPoint toks
             checkIOPoint $ C.resQueueTodo io res

      output toks =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resOutput = C.inputPoint io }

      ioPoint :: [S.TToken S.Chars] -> B.Ab (C.InputPoint S.Chars)
      ioPoint = paraInputPoint cd O.#. C.ttreePara2

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res' = let ins = C.resInput  res'
                              out = C.resOutput res'
                         in if out `elem` ins
                            then Msg.sameIOPoints out
                            else Right res'

      echo clause =
          Right $ res { C.resEcho = C.resEcho << (S.tChars O.<$$> B.clauseLines clause) }

      license line =
          Right $ res { C.resLicense = C.resLicense << (C.clauseSecNo h, line) }

calcContG :: (D.CContent c) => C.Global c -> D.CalcContent S.Chars c
calcContG = D.calcTree . D.getCops

paraInputPoint :: forall t. (O.Textual t) =>
    FilePath -> C.TTreePara t -> B.Ab (C.InputPoint t)
paraInputPoint cd = S.paraSelect unmatch ps where
    ps = [ (pJust1, just1)
         , (pStdin, stdin) ]

    pJust1 = S.paraSpec $ S.paraJust 1 . S.paraOpt ["about"]
    pStdin = S.paraSpec $ S.paraReq ["stdin"]

    just1 :: C.TTreePara t -> B.Ab (C.InputPoint t)
    just1 p = do arg   <- S.paraGetFst p
                 about <- S.paraGetOpt [] p "about"
                 case arg of
                   P.LText _ path -> let iop = B.ioPointDir cd $ B.ioPoint $ O.tString path
                                     in Right $ C.InputPoint iop about
                   _ -> Msg.adlib "input not text"

    stdin :: C.TTreePara t -> B.Ab (C.InputPoint t)
    stdin p = do args <- S.paraGet p "stdin"
                 case args of
                   [] -> Right $ C.InputPoint (B.IOPointStdin Nothing) []
                   _  -> Msg.adlib "input no args"

    unmatch = Msg.adlib "input unknown"

