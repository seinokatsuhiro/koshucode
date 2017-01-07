{-# OPTIONS_GHC -Wall #-}

-- | Ordered relation.

module Koshucode.Baala.Type.Rel.Order
  ( -- * Ordering
    relBodyOrder,

    -- * Portion
    RelPortion (..),
    nullPortion,
    takeEffectPortion,
    takePortion,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Judge            as T

-- | Sort relation body according to order specification.
relBodyOrder :: (Ord c, T.GetTermNames he) => [S.TermName] -> he -> O.Map [[c]]
relBodyOrder ns he = edit where
    edit  = B.sortByName ords $ T.getTermNames he
    ords  = (B.orderingCap . S.orderingTermName) <$> ns

-- | Portion of relation.
data RelPortion = RelPortion
    { portionOrder    :: [S.TermName]  -- ^ Order terms.
    , portionTop      :: Maybe Int     -- ^ Top /N/ tuples.
    , portionMiddle   :: Maybe Int     -- ^ Middle /N/ tuples.
    , portionBottom   :: Maybe Int     -- ^ Bottom /N/ tuples.
    , portionParts    :: [Int]         -- ^ Indecies of /N/ chunks.
    , portionPer      :: Maybe Int     -- ^ Divided /N/ chunks.
    } deriving (Show, Eq, Ord)

instance B.Default RelPortion where
    def = RelPortion { portionOrder   = []
                     , portionTop     = Nothing
                     , portionMiddle  = Nothing
                     , portionBottom  = Nothing
                     , portionParts   = []
                     , portionPer     = Nothing }

-- | Test portion is not effective.
nullPortion :: O.Test RelPortion
nullPortion RelPortion { portionTop     = Nothing
                       , portionMiddle  = Nothing
                       , portionBottom  = Nothing
                       , portionPer     = Nothing } = True
nullPortion _ = False

-- | Take effective (non-null) portion of relation.
takeEffectPortion :: (T.GetTermNames he, Ord c) => RelPortion -> he -> [[c]] -> [[c]]
takeEffectPortion p he | nullPortion p  = id
                       | otherwise      = takePortion p he

-- | Take portion of relation.
takePortion :: (T.GetTermNames he, Ord c) => RelPortion -> he -> [[c]] -> [[c]]
takePortion p@RelPortion { portionOrder = ord } he body
    = takePortionList p $ relBodyOrder ord he body

takePortionList :: (Ord c) => RelPortion -> [c] -> [c]
takePortionList p xs = xs' where
    xs' = B.unique (top p ++ mid p ++ bot p ++ parts p)

    top RelPortion { portionTop = Just n } = take n xs
    top _ = []

    mid RelPortion { portionMiddle = Just n } = B.takeMiddle n xs
    mid _ = []

    bot RelPortion { portionBottom = Just n } = B.takeEnd n xs
    bot _ = []

    parts RelPortion { portionParts = ps, portionPer = Just n } =
        concat $ B.takeChunks n ps xs
    parts _ = []

