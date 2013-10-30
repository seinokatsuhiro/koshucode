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
    => C.Dataset c          -- ^ Judges read from @source@ operator
    -> C.Relmap c           -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c              -- ^ Input relation
    -> B.AbortOr (B.Rel c)  -- ^ Output relation
runRelmapDataset = runRelmapViaRelfy . C.selectRelation

runRelmapViaRelfy
    ::  (Ord c)
    => C.RelSelect c
    -> C.Relmap c
    -> B.Rel c
    -> B.AbortOr (B.Rel c)
runRelmapViaRelfy sel m (B.Rel h1 b1) =
    do C.Relfy h2 f2 <- relmapRelfy sel m h1
       case C.relfy f2 b1 of
         Right b2 -> Right $ B.Rel h2 b2
         Left a   -> Left (a, [], [])

relmapRelfy
    :: C.RelSelect c
    -> C.Relmap c
    -> B.Relhead
    -> B.AbortOr (C.Relfy c)
relmapRelfy sel = (<$>) where
    C.RelmapSource _ p ns  <$> _  = Right $ C.relfyConst (sel p ns)
    C.RelmapConst  _ _ r   <$> _  = Right $ C.relfyConst r
    C.RelmapAlias  _ m     <$> h1 = m <$> h1
    C.RelmapName h op      <$> _  = left h $ B.AbortUnkRelmap op

    C.RelmapAppend m1 m2   <$> h1 =
        do relfy2 <- m1 <$> h1
           relfy3 <- m2 <$> C.relfyHead relfy2
           Right $ M.mappend relfy2 relfy3

    C.RelmapCalc h _ mk ms <$> h1 =
        do subrelfy <- (<$> h1) `mapM` ms
           case mk subrelfy h1 of
             Right relfy2 -> Right relfy2
             Left a       -> left h a

    left h a = Left (a, [], C.halfLines h)



-- ----------------------  Assert

{-| Calculate assertion list. -}
runAssertJudges
    :: (Ord c, C.CNil c)
    => [C.Assert c]          -- ^ Assertion list
    -> [B.Judge c]           -- ^ Input judges
    -> B.AbortOr [B.Judge c] -- ^ Output judges
runAssertJudges as = runAssertDataset as . C.dataset

{-| Calculate assertion list. -}
runAssertDataset
    :: (Ord c, C.CNil c)
    => [C.Assert c]
    -> C.Dataset c
    -> B.AbortOr [B.Judge c]
runAssertDataset as ds =
    do js <- mapM each as
       return $ concat js
    where each (C.Assert t pat opt r src) =
              do r1 <- runRelmapDataset ds r B.reldee
                 let q = C.assertQuality t
                 case assertOptionProcess q pat opt r1 of
                   Right js -> Right js
                   Left a   -> Left (a, [], src)

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
       else Left $ B.AbortUnkSymbol (fst . head $ rest)

{-| Get term name as string only if term is flat. -}
flatname :: B.TokenTree -> Maybe B.Termname
flatname (B.TreeL (B.TTerm _ [n])) = Just n
flatname _ = Nothing

flatnames :: [B.TokenTree] -> B.Ab [B.Termname]
flatnames trees =
    case mapM flatname trees of
      Just ns -> Right ns
      Nothing -> Left $ B.AbortMissingTermname ""



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
    | otherwise  = Left  $ B.AbortNoTerms non
    where
      non  =  B.headNonExistTerms h1 ns

      pos  :: [B.TermPos]
      pos  =  sort $ h1 `B.posFor` ns

      ind  :: [Int]
      ind  =  map B.posIndex pos

      h2   =  B.headChange   (ha ind) h1
      b2   =  B.unique $ map (ba ind) b1

