{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Relmap.Run
( runAssertJudges
, runAssertDataset
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.Assert     as C
import qualified Koshucode.Baala.Core.Relmap.Dataset    as C
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Relmap     as C



-- ----------------------  Relmap

{-| Calculate 'Relmap' for 'Rel'. -}
runRelmapDataset
    :: (Ord c, C.CNil c)
    => C.Dataset c          -- ^ Judges read from @source@ operator
    -> C.Relmap c           -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c              -- ^ Input relation
    -> B.AbortOr (B.Rel c)  -- ^ Output relation
runRelmapDataset ds = runRelmapSelector $ C.selectRelation ds

runRelmapSelector
    :: (Ord c, C.CNil c)
    => (B.JudgePattern -> [String] -> B.Rel c)  -- ^ Relation selector
    -> C.Relmap c              -- ^ Mapping from 'Rel' to 'Rel'
    -> B.Rel c                 -- ^ Input relation
    -> B.AbortOr (B.Rel c)     -- ^ Output relation
runRelmapSelector select = (<$>) where
    C.RelmapSource _ s ns <$> _  =  Right $ select s ns
    C.RelmapConst  _ _ r  <$> _  =  Right r
    C.RelmapAlias  _ m    <$> r  =  m <$> r
    C.RelmapAppend m1 m2  <$> r  =  (m1 <$> r) >>= (m2 <$>)
    C.RelmapName   h op   <$> _  =  Left (B.AbortUnkRelmap op,
                                           [], C.halfLines h)
    C.RelmapCalc h _ f ms <$> r  =
        do rs' <- mapM (<$> r) ms
           case f rs' r of
             Right r' -> Right r'
             Left a   -> Left (a, [], C.halfLines h)



-- ----------------------  Assert

{-| Calculate assertion list. -}
runAssertJudges
    :: (Ord c, C.CNil c)
    => [C.Assert c]          -- ^ Assertion list
    -> [B.Judge c]           -- ^ Input judges
    -> B.AbortOr [B.Judge c] -- ^ Output judges
runAssertJudges as = runAssertDataset as . C.dataset

{-| Calculate assertion list. -}
runAssertDataset ::
    (Ord c, C.CNil c) => [C.Assert c] -> C.Dataset c -> B.AbortOr [B.Judge c]
runAssertDataset as ds =
    do js <- mapM each as
       return $ concat js
    where each (C.Assert q pat opt r src) =
              do r1 <- runRelmapDataset ds r B.reldee
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

flatnames :: [B.TokenTree] -> B.Ab [B.Termname]
flatnames trees =
    case mapM B.flatname trees of
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
       B.arrangeRelRaw B.arrangeFore B.arrangeFore ns r1

assertOptionOrder :: (Ord c) => [B.TokenTree] ->  B.AbMap (B.Rel c)
assertOptionOrder _ r1 = Right r1

assertOptionJudges :: C.AssertOption -> [B.Judge c] -> B.Ab [B.Judge c]
assertOptionJudges _ js = Right js

