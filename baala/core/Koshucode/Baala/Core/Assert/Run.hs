{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Assert.Run
  ( runAssertJudges,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert.Assert   as C
import qualified Koshucode.Baala.Core.Assert.Dataset  as C
import qualified Koshucode.Baala.Core.Assert.RelTable as C
import qualified Koshucode.Baala.Core.Message         as Msg


-- ----------------------  Assert

-- | Calculate assertion list.
runAssertJudges :: (Ord c, B.Write c, C.CRel c, C.CEmpty c)
  => C.Global h c -> C.ShortAssert' h c -> B.Ab (B.OutputChunks c)
runAssertJudges global a@(B.Short pt sh _) =
    do chunks <- runAssertDataset global a ds
       Right $ B.Short pt sh chunks
    where ds = C.dataset $ C.globalJudges global

-- | Calculate assertion list.
runAssertDataset :: forall h. forall c. (Ord c, B.Write c, C.CRel c, C.CEmpty c)
  => C.Global h c -> C.ShortAssert' h c -> C.Dataset c -> B.Ab [B.OutputChunk c]
runAssertDataset global (B.Short _ sh asserts) dataset =
    Right . concat =<< mapM each asserts
    where
      each (C.Assert _ _ _ _ _ _ Nothing _) = B.bug "runAssertDataset"
      each a@(C.Assert _ typ pat opt _ _ (Just relmap) libs) =
          Msg.abAssert [a] $ do
            r1 <- runRelmapDataset global dataset libs relmap B.reldee
            let judgeOf showEmpty = assert showEmpty typ
            optionProcess sh judgeOf pat opt r1

      assert :: (C.CEmpty c) => Bool -> C.AssertType -> B.JudgeOf c
      assert True  q p = C.assertAs q p
      assert False q p = C.assertAs q p . omitEmpty

      omitEmpty :: (C.CEmpty c) => B.Map [B.Named c]
      omitEmpty =  B.omit (C.isEmpty . snd)

-- | Calculate 'Relmap' for 'Rel'.
runRelmapDataset
    :: (Ord c, C.CRel c, C.CEmpty c)
    => C.Global h c
    -> C.Dataset c          -- ^ Judges read from @source@ operator
    -> C.RelmapLinkTable h c
    -> C.Relmap h c         -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c              -- ^ Input relation
    -> B.Ab (B.Rel c)       -- ^ Output relation
runRelmapDataset global dataset = runRelmapViaRelkit g2 where
    g2 = global { C.globalSelect = C.selectRelation dataset }

runRelmapViaRelkit :: (Ord c, C.CRel c)
  => C.Global h c -> C.RelmapLinkTable h c
  -> C.Relmap h c -> B.AbMap (B.Rel c)
runRelmapViaRelkit g2 links r (B.Rel he1 bo1) =
    do (kdef, C.Relkit he2' f2') <- C.relmapSpecialize g2 links [] (Just he1) r
       let C.Relkit mhe2 f2 = C.relkitLink kdef $ C.Relkit he2' f2'
       he2 <- just "unknown relhead" mhe2
       bo2 <- C.relkitRun g2 [] f2 bo1
       Right $ B.Rel he2 bo2
    where
      just :: String -> Maybe a -> B.Ab a
      just _ (Just h) = Right h
      just s Nothing  = Msg.adlib s


-- ---------------------------------  Options

optionList :: [String]
optionList = [ "-empty"  -- show empty filler
             , "-fore"   -- move terms to front
             , "-order"  -- sort list of judges by content
             , "-align"  -- align terms vertically
             , "-table"  -- output relation in tabular format
             ]

optionProcess :: (Ord c, B.Write c, C.CRel c)
    => [B.ShortDef] -> (Bool -> B.JudgeOf c) -> B.JudgePat -> C.AssertOption
    -> B.Rel c -> B.Ab [B.OutputChunk c]
optionProcess sh judgeOf pat opt r1 =
    do optionCheck optionList opt
       r2 <- optionRelmap opt r1
       let showEmpty = "-empty" `optionElem` opt
           judges    = B.judgesFromRel (judgeOf showEmpty) pat r2
           comment   = optionComment sh pat opt r2
       Right [ B.OutputJudge judges, B.OutputComment comment ]

optionCheck :: [String] -> [B.NamedTrees] -> B.Ab ()
optionCheck ns xs =
    let rest = B.assocCut ("@trunk" : ns) xs
    in if null rest
       then Right ()
       else Msg.unkWord (fst . head $ rest)

optionRelmap :: (Ord c, C.CRel c) => C.AssertOption -> B.AbMap (B.Rel c)
optionRelmap opt r1 =
    Right r1 >>= call optionFore  "-fore"
             >>= call optionOrder "-order"
    where call f name r2 = case lookup name opt of
                             Nothing   -> Right r2
                             Just opt2 -> f opt2 r2

optionComment :: (B.Write c, C.CRel c) =>
    [B.ShortDef] -> B.JudgePat -> C.AssertOption -> B.Rel c -> [String]
optionComment sh p opt r =
    case "-table" `optionElem` opt of
      True  -> title : "" : table
      False -> []
    where
      title = "TABLE : " ++ p
      table = ("  " ++) `map` C.relTableLines sh r

optionElem :: String -> [B.NamedTrees] -> Bool
optionElem name opt = name `elem` map fst opt


-- ---------------------------------  Option "-fore"

optionFore :: (Ord c) => [B.TTree] -> B.AbMap (B.Rel c)
optionFore opt2 r1 =
    do ns <- flatnames opt2
       snipRelRaw B.snipFore2 ns r1

flatnames :: [B.TTree] -> B.Ab [B.TermName]
flatnames trees =
    case mapM flatname trees of
      Just ns -> Right ns
      Nothing -> Msg.reqTermName

-- | Get term name as string only if term is flat.
flatname :: B.TTree -> Maybe B.TermName
flatname (B.TermLeaf _ _ [n])  = Just n
flatname _                     = Nothing

snipRelRaw :: (Ord c) => B.SnipPair B.NamedType c -> [B.TermName] -> B.AbMap (B.Rel c)
snipRelRaw (heSnip, boSnip) ns (B.Rel he1 bo1)
    | null left  = Right r2
    | otherwise  = Msg.unkTerm left he1
    where
      ns1  =  B.headNames he1
      ind  =  ns `B.snipIndex` ns1
      left =  ns `B.snipLeft`  ns1

      r2   =  B.Rel he2 bo2
      he2  =  heSnip ind `B.headMap` he1
      bo2  =  boSnip ind `map` bo1


-- ---------------------------------  Option "-order"

optionOrder :: (Ord c, C.CRel c) => [B.TTree] ->  B.AbMap (B.Rel c)
optionOrder _ r1 = Right $ relSortDeep r1

relSortDeep :: (Ord c, C.CRel c) => B.Map (B.Rel c)
relSortDeep = relApply f where
    f (B.Rel he bo) = B.Rel he $ B.sort bo

relApply :: (C.CRel c) => B.Map (B.Map (B.Rel c))
relApply f (B.Rel he bo) = f $ B.Rel he $ B.map2 nest bo where
    nest c | C.isRel c = C.pRel $ relApply f $ C.gRel c
           | otherwise = c

