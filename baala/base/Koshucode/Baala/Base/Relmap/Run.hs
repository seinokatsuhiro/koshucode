{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Base.Relmap.Run
( runAssertJudges
, runAssertDataset
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Content

import Koshucode.Baala.Base.Relmap.Assert
import Koshucode.Baala.Base.Relmap.Dataset
import Koshucode.Baala.Base.Relmap.HalfRelmap
import Koshucode.Baala.Base.Relmap.Relmap



-- ----------------------  Relmap

{-| Calculate 'Relmap' for 'Rel'. -}
runRelmapDataset
    :: (Ord v, Nil v)
    => Dataset v        -- ^ Judges read from @source@ operator
    -> Relmap v         -- ^ Mapping from 'Rel' to 'Rel'
    -> Rel v            -- ^ Input relation
    -> AbortOr (Rel v)  -- ^ Output relation
runRelmapDataset ds = runRelmapSelector $ selectRelation ds

runRelmapSelector
    :: (Ord v, Nil v)
    => (Relsign -> [String] -> Rel v)  -- ^ Relation selector
    -> Relmap v          -- ^ Mapping from 'Rel' to 'Rel'
    -> Rel v             -- ^ Input relation
    -> AbortOr (Rel v)   -- ^ Output relation
runRelmapSelector select = (<$>) where
    RelmapSource _ s ns   <$> _ = Right $ select s ns
    RelmapConst  _ _ r    <$> _ = Right r
    RelmapAlias  _ m      <$> r = m <$> r
    RelmapCalc _ _ f ms   <$> r = do rs' <- mapM (<$> reldee) ms
                                     Right $ f rs' r
    RelmapAbCalc h _ f ms <$> r = do rs' <- mapM (<$> reldee) ms
                                     case f rs' r of
                                       Right r' -> Right r'
                                       Left a   -> Left $ a (halfLines h)
    RelmapAppend m1 m2    <$> r = do r' <- m1 <$> r
                                     m2 <$> r'
    RelmapName   h op     <$> _ = Left (AbortUnknownRelmap op, halfLines h)



-- ----------------------  Assert

{-| Calculate assertion list. -}
runAssertJudges
    :: (Ord v, Nil v)
    => [Assert v]        -- ^ Assertion list
    -> [Judge v]         -- ^ Input judges
    -> AbortOr [Judge v] -- ^ Output judges
runAssertJudges as = runAssertDataset as . dataset

{-| Calculate assertion list. -}
runAssertDataset ::
    (Ord v, Nil v) => [Assert v] -> Dataset v -> AbortOr [Judge v]
runAssertDataset as ds = judges where
    judges = do
      js <- mapM each as
      return $ concat js
    each (Assert q s m) = do
      r <- runRelmapDataset ds m reldee
      return $ judgesFromRel q s r

{-| Convert relation to list of judges -}
judgesFromRel :: Bool -> Relsign -> Rel v -> [Judge v]
judgesFromRel q s = judges where
    judges (Rel h b) = map (judge h) b
    judge h = Judge q s . zip (headNames h)

