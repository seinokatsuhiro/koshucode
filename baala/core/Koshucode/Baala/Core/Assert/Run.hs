{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Assert.Run
( runAssertJudges,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content        as C
import qualified Koshucode.Baala.Core.Relmap         as C
import qualified Koshucode.Baala.Core.Assert.Assert  as C
import qualified Koshucode.Baala.Core.Assert.Dataset as C
import qualified Koshucode.Baala.Core.Message        as Message



-- ----------------------  Relmap

-- | Calculate 'Relmap' for 'Rel'.
runRelmapDataset
    :: (Ord c, C.CRel c, C.CNil c)
    => C.Global c
    -> C.Dataset c     -- ^ Judges read from @source@ operator
    -> [C.RelmapDef c]
    -> C.Relmap c      -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c         -- ^ Input relation
    -> B.Ab (B.Rel c)  -- ^ Output relation
runRelmapDataset global dataset rdef = runRelmapViaRelkit g2 rdef where
    g2 = global { C.globalSelect = C.selectRelation dataset }

runRelmapViaRelkit :: (Ord c, C.CRel c)
  => C.Global c -> [C.RelmapDef c]
  -> C.Relmap c -> B.AbMap (B.Rel c)
runRelmapViaRelkit global rdef r (B.Rel he1 bo1) =
    do (kdef, C.Relkit he2' f2') <- C.relmapSpecialize global rdef [] (Just he1) r
       let C.Relkit mhe2 f2 = C.relkitLink kdef $ C.Relkit he2' f2'
       he2 <- justRelhead mhe2
       bo2 <- C.relkitRun [] f2 bo1
       Right $ B.Rel he2 bo2

justRelhead :: Maybe B.Relhead -> B.Ab B.Relhead
justRelhead = just "unknown relhead"

just :: String -> Maybe a -> B.Ab a
just _ (Just h) = Right h
just s Nothing  = Message.adlib s

-- ----------------------  Assert

-- | Calculate assertion list.
runAssertJudges :: (Ord c, C.CRel c, C.CNil c)
  => C.Global c -> [C.RelmapDef c] -> [C.Assert c] -> B.Ab [B.Judge c]
runAssertJudges global rdef asserts =
    runAssertDataset global asserts ds rdef where
        ds = C.dataset $ C.globalJudges global

-- | Calculate assertion list.
runAssertDataset :: (Ord c, C.CRel c, C.CNil c)
  => C.Global c -> [C.Assert c] -> C.Dataset c -> [C.RelmapDef c] -> B.Ab [B.Judge c]
runAssertDataset global asserts dataset rdef = Right . concat =<< mapM each asserts where
    each a@(C.Assert quo pat opt relmap _) =
        B.abortableFrom "assert" a $ do
          r1 <- runRelmapDataset global dataset rdef relmap B.reldee
          let q = C.assertQuality quo
          assertOptionProcess q pat opt r1

-- | Convert relation to list of judges.
judgesFromRel :: Bool -> B.JudgePattern -> B.Rel c -> [B.Judge c]
judgesFromRel q pat = judges where
    judges (B.Rel h b) = map (judge h) b
    judge h = B.Judge q pat . zip (B.headNames h)

optionUnkCheck :: [String] -> [B.Named [B.TokenTree]] -> B.Ab ()
optionUnkCheck ns xs =
    let rest = B.assocCut ("" : ns) xs
    in if null rest
       then Right ()
       else Message.unkWord (fst . head $ rest)

-- | Get term name as string only if term is flat.
flatname :: B.TokenTree -> Maybe B.TermName
flatname (B.TreeL (B.TTerm _ [n])) = Just n
flatname _ = Nothing

flatnames :: [B.TokenTree] -> B.Ab [B.TermName]
flatnames trees =
    case mapM flatname trees of
      Just ns -> Right ns
      Nothing -> Message.reqTermName



-- ---------------------------------  Option

assertOptionProcess :: (Ord c)
  => Bool -> B.JudgePattern -> C.AssertOption -> B.Rel c -> B.Ab [B.Judge c]
assertOptionProcess q pat opt r1 =
    do assertOptionCheck opt
       r2 <- assertOptionRelmap opt r1
       let js = judgesFromRel q pat r2
       assertOptionJudges opt js

assertOptionCheck :: C.AssertOption -> B.Ab ()
assertOptionCheck = optionUnkCheck ["-fore", "-order", "-align", "-table"]

assertOptionRelmap :: (Ord c) => C.AssertOption -> B.Rel c -> B.Ab (B.Rel c)
assertOptionRelmap opt r1 =
    Right r1  >>= call "-fore"  assertOptionFore
              >>= call "-order" assertOptionOrder
    where call name f r =
              case lookup name opt of
                Just opt2 -> f opt2 r
                Nothing   -> Right r

assertOptionFore :: (Ord c) => [B.TokenTree] -> B.AbMap (B.Rel c)
assertOptionFore opt r1 =
    do ns <- flatnames opt
       snipRelRaw B.snipFore2 ns r1

assertOptionOrder :: (Ord c) => [B.TokenTree] ->  B.AbMap (B.Rel c)
assertOptionOrder _ r1 = Right r1

assertOptionJudges :: C.AssertOption -> [B.Judge c] -> B.Ab [B.Judge c]
assertOptionJudges _ js = Right js

snipRelRaw :: (Ord c) => B.SnipPair B.TermName c -> [B.TermName] -> B.AbMap (B.Rel c)
snipRelRaw (heSnip, boSnip) ns (B.Rel he1 bo1)
    | null left  = Right r2
    | otherwise  = Message.unkTerm left he1
    where
      ns1  =  B.headNames he1
      ind  =  ns `B.snipIndex` ns1
      left =  ns `B.snipLeft`  ns1

      r2   =  B.Rel he2 bo2
      he2  =  B.headFrom $ heSnip ind       ns1
      bo2  =               boSnip ind `map` bo1

