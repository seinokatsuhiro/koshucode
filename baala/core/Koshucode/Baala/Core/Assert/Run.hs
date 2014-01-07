{-# OPTIONS_GHC -Wall #-}

{-| Running relational calculation. -}

module Koshucode.Baala.Core.Assert.Run
( runAssertJudges,
  runAssertDataset,
) where

import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content        as C
import qualified Koshucode.Baala.Core.Relmap         as C
import qualified Koshucode.Baala.Core.Assert.Assert  as C
import qualified Koshucode.Baala.Core.Assert.Dataset as C



-- ----------------------  Relmap

{-| Calculate 'Relmap' for 'Rel'. -}
runRelmapDataset
    :: (Ord c, C.CNil c)
    => C.Global c
    -> C.Dataset c     -- ^ Judges read from @source@ operator
    -> C.Relmap c      -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c         -- ^ Input relation
    -> B.Ab (B.Rel c)  -- ^ Output relation
runRelmapDataset global dataset = runRelmapViaRelfy g2 where
    g2 = global { C.globalSelect = C.selectRelation dataset }

runRelmapViaRelfy :: (Ord c) => C.Global c -> C.Relmap c -> B.AbMap (B.Rel c)
runRelmapViaRelfy global r (B.Rel h1 b1) =
    do C.Relfy h2 f2 <- specialize global r h1
       b2 <- C.relfyRun f2 b1
       Right $ B.Rel h2 b2

specialize :: C.Global c -> C.Relmap c -> B.Relhead -> B.Ab (C.Relfy c)
specialize global = (<$>) where
    sel = C.globalSelect global

    C.RelmapSource half p ns <$> _  = right half (C.relfyConst $ sel p ns)
    C.RelmapConst  half rel  <$> _  = right half (C.relfyConst rel)
    C.RelmapAlias  _ relmap  <$> h1 = relmap <$> h1
    C.RelmapName   half name <$> _  =
        B.abFrom half $ Left $ B.AbortAnalysis [] $ B.AAUnkRelmap name

    C.RelmapAppend relmap1 relmap2 <$> h1 =
        do relfy2 <- relmap1 <$> h1
           relfy3 <- relmap2 <$> C.relfyHead relfy2
           Right $ M.mappend relfy2 relfy3

    C.RelmapCalc half mk relmaps <$> h1 =
        B.abFrom half $ do
          subrelfy <- (<$> h1) `mapM` relmaps
          relfy    <- mk subrelfy h1
          right half relfy

    C.RelmapGlobal half mk <$> h1 =
        B.abFrom half $ do
          relfy <- mk global h1
          right half relfy

    right half r = Right $ C.relfySetSource half $ r



-- ----------------------  Assert

{-| Calculate assertion list. -}
runAssertJudges :: (Ord c, C.CNil c) => C.Global c -> [C.Assert c] -> B.Ab [B.Judge c]
runAssertJudges global asserts =
    runAssertDataset global asserts $ C.dataset $ C.globalJudges global

{-| Calculate assertion list. -}
runAssertDataset :: (Ord c, C.CNil c) => C.Global c -> [C.Assert c] -> C.Dataset c -> B.Ab [B.Judge c]
runAssertDataset global asserts dataset = Right . concat =<< mapM each asserts where
    each (C.Assert t pat opt r src) =
        B.ab src $ do
          r1 <- runRelmapDataset global dataset r B.reldee
          let q = C.assertQuality t
          assertOptionProcess q pat opt r1

{-| Convert relation to list of judges -}
judgesFromRel :: Bool -> B.JudgePattern -> B.Rel c -> [B.Judge c]
judgesFromRel q pat = judges where
    judges (B.Rel h b) = map (judge h) b
    judge h = B.Judge q pat . zip (B.headNames h)

optionUnkCheck :: [String] -> [B.Named [B.TokenTree]] -> B.Ab ()
optionUnkCheck ns xs =
    let rest = B.assocOmitAll ("" : ns) xs
    in if null rest
       then Right ()
       else Left $ B.AbortSyntax [] $ B.ASUnkWord (fst . head $ rest)

{-| Get term name as string only if term is flat. -}
flatname :: B.TokenTree -> Maybe B.Termname
flatname (B.TreeL (B.TTerm _ [n])) = Just n
flatname _ = Nothing

flatnames :: [B.TokenTree] -> B.Ab [B.Termname]
flatnames trees =
    case mapM flatname trees of
      Just ns -> Right ns
      Nothing -> Left $ B.AbortAnalysis [] $ B.AAMissingTermname ""



-- ---------------------------------  Option

assertOptionProcess :: (Ord c) => Bool -> B.JudgePattern -> C.AssertOption -> B.Rel c -> B.Ab [B.Judge c]
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
       arrangeRelRaw B.arrangeFore B.arrangeFore ns r1

assertOptionOrder :: (Ord c) => [B.TokenTree] ->  B.AbMap (B.Rel c)
assertOptionOrder _ r1 = Right r1

assertOptionJudges :: C.AssertOption -> [B.Judge c] -> B.Ab [B.Judge c]
assertOptionJudges _ js = Right js

arrangeRelRaw
    :: (Ord c)
    => B.Arrange B.Termname  -- ^ Arranger for termnames,
                             --   e.g., 'B.arrangePick', 'B.arrangeCut', etc
    -> B.Arrange c           -- ^ Arranger for term contents
    -> [B.Termname]          -- ^ Names of terms
    -> B.AbMap (B.Rel c)     -- ^ Relation-to-relation mapping
arrangeRelRaw = arrangeRelUsing id

arrangeRelUsing
    :: (Ord c)
    => B.Map [B.TermPos]
    -> B.Arrange B.Termname
    -> B.Arrange c
    -> [B.Termname]
    -> B.AbMap (B.Rel c)
arrangeRelUsing sort ha ba ns (B.Rel h1 b1)
    | null non   = Right $ B.Rel h2 b2
    | otherwise  = Left  $ B.AbortAnalysis [] (B.AANoTerms non)
    where
      non  =  B.headDropTerms h1 ns

      pos  :: [B.TermPos]
      pos  =  sort $ h1 `B.posFor` ns

      ind  :: [Int]
      ind  =  map B.posIndex pos

      h2   =  B.headChange   (ha ind) h1
      b2   =  B.unique $ map (ba ind) b1

