{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Assert.Run
  ( runAssertJudges,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relkit          as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert.Assert   as C
import qualified Koshucode.Baala.Core.Assert.RelTable as C
import qualified Koshucode.Baala.Core.Message         as Msg


-- ----------------------  Assert

-- | Calculate assertion list.
runAssertJudges :: (Ord c, B.Write c, C.CRel c, C.CEmpty c, B.SelectRel h, C.GetGlobal h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab (B.ResultShortChunks String)
runAssertJudges hook opt a =
    do chunks <- runAssertDataset hook opt a
       Right $ a { B.shortBody = chunks }

-- | Calculate assertion list.
runAssertDataset :: forall h. forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c, B.SelectRel h, C.GetGlobal h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab [B.ResultChunk String]
runAssertDataset hook option (B.Short _ sh ass) =
    Right . concat =<< mapM each ass
    where
      each (C.Assert _ _ _ _ _ Nothing _) = B.bug "runAssertDataset"
      each a@(C.Assert _ typ pat _ opt (Just relmap) libs) =
          Msg.abAssert [a] $ do
            r1 <- runRelmapViaRelkit hook libs relmap B.reldee
            let judgeOf showEmpty = assert showEmpty typ
            optionProcess sh judgeOf pat option opt r1

      assert :: (C.CEmpty c) => Bool -> C.AssertType -> B.JudgeOf c
      assert True  q p = C.assertAs q p
      assert False q p = C.assertAs q p . omitEmpty

      omitEmpty :: (C.CEmpty c) => B.Map [B.Term c]
      omitEmpty =  B.omit (C.isEmpty . snd)

runRelmapViaRelkit :: (Ord c, C.CRel c, B.SelectRel h, C.GetGlobal h)
  => h c -> C.RelmapLinkTable' h c
  -> C.Relmap' h c -> B.AbMap (B.Rel c)
runRelmapViaRelkit hook links r (B.Rel he1 bo1) =
    do (kdef, C.Relkit hi2' ho2' f2') <- C.relmapSpecialize hook links [] (Just he1) r
       let C.Relkit _ mhe2 f2 = C.relkitLink kdef $ C.Relkit hi2' ho2' f2'
       he2 <- just "unknown relhead" mhe2
       bo2 <- C.relkitRun hook [] f2 bo1
       Right $ B.Rel he2 bo2
    where
      just :: String -> Maybe a -> B.Ab a
      just _ (Just h) = Right h
      just s Nothing  = Msg.adlib s


-- ---------------------------------  Options

optionType :: B.ParaType String
optionType = B.paraType `B.paraMin` 0 `B.paraOpt`
             [ "empty"     -- show empty filler
             , "forward"   -- move terms to front
             , "backward"  -- move terms to rear
             , "lexical"   -- make terms lexical order
             , "order"     -- sort list of judges by content
             , "align"     -- align terms vertically
             , "table"     -- output relation in tabular format
             ]

optionProcess :: (Ord c, B.Write c, C.CRel c)
    => [B.ShortDef] -> (Bool -> B.JudgeOf c) -> B.JudgePat
    -> C.Option c -> C.TTreePara
    -> B.Rel c -> B.Ab [B.ResultChunk String]
optionProcess sh judgeOf pat option opt r1 =
    do case B.paraUnmatch opt optionType of
         Nothing  -> Right ()
         Just un  -> Msg.unkOption un
       showEmpty <- B.paraGetSwitch opt "empty"
       r2 <- optionRelmapResource option r1
       r3 <- optionRelmapAssert   opt    r2
       let sh' = B.shortText sh
           js  = B.judgesFromRel (judgeOf showEmpty) pat r3
           js' = B.textualjudge sh' `map` js
           r4  = B.writeString sh' `fmap` r3
       comment <- optionComment sh pat opt r3
       Right [ B.ResultJudge js'
             , B.ResultRel pat r4
             , B.ResultNote comment ]

optionRelmapResource :: (Ord c, C.CRel c) => C.Option c -> B.AbMap (B.Rel c)
optionRelmapResource option r1 =
    Right r1 >>= call optionOrder (C.optionBool "order" option)
    where call f True  r2 = f [] r2
          call _ False r2 = Right r2

optionRelmapAssert :: (Ord c, C.CRel c) => C.TTreePara -> B.AbMap (B.Rel c)
optionRelmapAssert opt r1 =
    Right r1 >>= call optionForward  "forward"
             >>= call optionBackward "backward"
             >>= call optionLexical  "lexical"
             >>= call optionOrder    "order"
    where call f name r2 = case B.paraGet opt name of
                             Right args -> f args r2
                             Left _     -> Right r2

optionComment :: (B.Write c, C.CRel c) =>
    [B.ShortDef] -> B.JudgePat -> C.TTreePara -> B.Rel c -> B.Ab [String]
optionComment sh p opt r =
    do optTable <- B.paraGetSwitch opt "table"
       case optTable of
         True  -> Right $ title : "" : table
         False -> Right []
    where
      title = "TABLE : " ++ p
      table = ("  " ++) `map` C.relTableLines sh r


-- ---------------------------------  Option "forward" and "backward"

type SnipRel c = B.Snip2 B.NamedType c

optionForward, optionBackward :: (Ord c) => [B.TTree] -> B.AbMap (B.Rel c)
optionForward  = optionToward B.snipForward2
optionBackward = optionToward B.snipBackward2

optionToward :: (Ord c) => SnipRel c -> [B.TTree] -> B.AbMap (B.Rel c)
optionToward snip2 opt2 r1 =
    do ns <- flatnames opt2
       snipRel snip2 ns r1

flatnames :: [B.TTree] -> B.Ab [B.TermName]
flatnames trees =
    case mapM flatname trees of
      Just ns -> Right ns
      Nothing -> Msg.reqTermName

-- | Get term name as string only if term is flat.
flatname :: B.TTree -> Maybe B.TermName
flatname (B.TermLeafName _ n)  = Just n
flatname (B.TermLeaf _ _ [n])  = Just n
flatname _                     = Nothing

snipRel :: (Ord c) => SnipRel c -> [B.TermName] -> B.AbMap (B.Rel c)
snipRel (heSnip, boSnip) ns (B.Rel he1 bo1)
    | null left  = Right r2
    | otherwise  = Msg.unkTerm left he1
    where
      ns1   = B.headNames he1
      ind   = ns `B.snipIndex` ns1
      left  = ns `B.snipLeft`  ns1

      r2    = B.Rel he2 bo2
      he2   = heSnip ind `B.headMap` he1
      bo2   = boSnip ind `map` bo1

optionLexical :: (Ord c) => [B.TTree] -> B.AbMap (B.Rel c)
optionLexical _ = lexicalOrderRel

lexicalOrderRel :: (Ord c) => B.AbMap (B.Rel c)
lexicalOrderRel rel@(B.Rel he1 _) = snipRel B.snipForward2 ns' rel where
    ns' = B.sort $ B.headNames he1


-- ---------------------------------  Option "order"

optionOrder :: (Ord c, C.CRel c) => [B.TTree] ->  B.AbMap (B.Rel c)
optionOrder _ r1 = Right $ relSortDeep r1

relSortDeep :: (Ord c, C.CRel c) => B.Map (B.Rel c)
relSortDeep = relApply f where
    f (B.Rel he bo) = B.Rel he $ B.sort bo

relApply :: (C.CRel c) => B.Map (B.Map (B.Rel c))
relApply f (B.Rel he bo) = f $ B.Rel he $ B.map2 nest bo where
    nest c | C.isRel c = C.pRel $ relApply f $ C.gRel c
           | otherwise = c

