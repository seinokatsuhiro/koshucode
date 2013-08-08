{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Relmap.Run
( runAssertJudges
, runAssertDataset
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap.Assert
import Koshucode.Baala.Core.Relmap.Dataset
import Koshucode.Baala.Core.Relmap.HalfRelmap
import Koshucode.Baala.Core.Relmap.Relmap



-- ----------------------  Relmap

{-| Calculate 'Relmap' for 'Rel'. -}
runRelmapDataset
    :: (Ord c, CNil c)
    => Dataset c        -- ^ Judges read from @source@ operator
    -> Relmap c         -- ^ Mapping from 'Rel' to 'Rel'
    -> Rel c            -- ^ Input relation
    -> AbortOr (Rel c)  -- ^ Output relation
runRelmapDataset ds = runRelmapSelector $ selectRelation ds

runRelmapSelector
    :: (Ord c, CNil c)
    => (Relsign -> [String] -> Rel c)  -- ^ Relation selector
    -> Relmap c          -- ^ Mapping from 'Rel' to 'Rel'
    -> Rel c             -- ^ Input relation
    -> AbortOr (Rel c)   -- ^ Output relation
runRelmapSelector select = (<$>) where
    RelmapSource _ s ns   <$> _ = Right $ select s ns
    RelmapConst  _ _ r    <$> _ = Right r
    RelmapAlias  _ m      <$> r = m <$> r
    RelmapCalc h _ f ms   <$> r = do rs' <- mapM (<$> reldee) ms
                                     case f rs' r of
                                       Right r' -> Right r'
                                       Left a   -> Left (a, halfLines h)
    RelmapAppend m1 m2    <$> r = do r' <- m1 <$> r
                                     m2 <$> r'
    RelmapName   h op     <$> _ = Left (AbortUnknownRelmap op, halfLines h)



-- ----------------------  Assert

{-| Calculate assertion list. -}
runAssertJudges
    :: (Ord c, CNil c)
    => [Assert c]        -- ^ Assertion list
    -> [Judge c]         -- ^ Input judges
    -> AbortOr [Judge c] -- ^ Output judges
runAssertJudges as = runAssertDataset as . dataset

{-| Calculate assertion list. -}
runAssertDataset ::
    (Ord c, CNil c) => [Assert c] -> Dataset c -> AbortOr [Judge c]
runAssertDataset as ds = judges where
    judges = do
      js <- mapM each as
      return $ concat js
    each (Assert q s m) = do
      r <- runRelmapDataset ds m reldee
      return $ judgesFromRel q s r

{-| Convert relation to list of judges -}
judgesFromRel :: Bool -> Relsign -> Rel c -> [Judge c]
judgesFromRel q s = judges where
    judges (Rel h b) = map (judge h) b
    judge h = Judge q s . zip (headNames h)

