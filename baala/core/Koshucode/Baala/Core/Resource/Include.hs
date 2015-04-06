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
           f `to` upd = Msg.abClause toks $ do
                          x <- f h toks b
                          Right $ (upd x) { C.resLastSecNo = sec }
       case b of
         C.CJudge   _ _ _ -> judge  `to` \x -> res { C.resJudge   = C.resJudge   << x }
         C.CAssert  _ _ _ -> assert `to` \x -> res { C.resAssert  = C.resAssert  << x }
         C.CRelmap  _ _   -> relmap `to` \x -> res { C.resLexmap  = C.resLexmap  << x }
         C.CSlot    _ _   -> slot   `to` \x -> res { C.resSlot    = C.resSlot    << x }
         C.CInput   _     -> input  `to` id
         C.COutput  _     -> output `to` id
         C.COption  _     -> option `to` \x -> res { C.resOption  = x }
    where
      f << y  = y : f res
      f <: t  = case f res of (todo1, todo2, done) -> (t : todo1, todo2, done)

      judge :: Clab (B.Judge c)
      judge _ _ (C.CJudge q p toks) =
          C.treesToJudge calc q p =<< B.ttrees toks

      calc :: C.CalcContent c
      calc = calcContG $ C.resGlobal res

      assert :: Clab (C.ShortAssert' h c)
      assert C.ClauseHead { C.clauseSecNo = sec, C.clauseShort = sh }
                          src (C.CAssert typ pat toks) =
          do optPara <- C.ttreePara2 toks
             let ass  = C.Assert sec typ pat src optPara Nothing []
             Right $ B.Short (B.codePtList $ head src) sh ass

      relmap :: Clab C.LexmapClause
      relmap C.ClauseHead { C.clauseSecNo = sec } _ (C.CRelmap n toks) =
          do lt <- C.consLexmapTrees =<< C.ttreePara2 toks
             Right ((sec, n), lt)

      slot :: Clab B.NamedTrees
      slot _ _ (C.CSlot n toks) =
          do trees <- B.ttrees toks
             Right (n, trees)

      option :: Clab (C.Option c)
      option _ _ (C.COption toks) = C.optionParse calc toks (C.resOption res)

      input :: Clab (C.Resource c)
      input _ _ (C.CInput toks) =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resInput = C.resInput <: io }

      output :: Clab (C.Resource c)
      output _ _ (C.COutput toks) =
          do io <- ioPoint toks
             checkIOPoint $ res { C.resOutput = io }

      ioPoint :: [B.Token] -> B.Ab B.IOPoint
      ioPoint = C.ttreePara2 B.>=> paraToIOPoint

      checkIOPoint :: B.AbMap (C.Resource c)
      checkIOPoint res' = let (ins, out) = C.resIOPoints res'
                          in if out `elem` ins
                             then Msg.sameIOPoints out
                             else Right res'

coxBuildG :: (C.CContent c) => C.Global c -> B.TTreeToAb (C.Cox c)
coxBuildG g = C.coxBuild (calcContG g) (C.globalCopset g)

calcContG :: (C.CContent c) => C.Global c -> C.CalcContent c
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

