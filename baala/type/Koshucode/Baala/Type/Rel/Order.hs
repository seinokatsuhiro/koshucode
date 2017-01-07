{-# OPTIONS_GHC -Wall #-}

-- | Ordered relation.

module Koshucode.Baala.Type.Rel.Order
  ( -- * Ordering
    relBodyOrder,

    -- * Portion of list
    Portion (..),
    nullPortion,
    takePortion,

    -- * Portion of relation
    RelPortion,
    takeRelPortion,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Judge            as T


-- ============================================  Ordering

-- | Sort relation body according to order specification.
relBodyOrder :: (Ord c, T.GetTermNames he) => [S.TermName] -> he -> O.Map [[c]]
relBodyOrder ns he = edit where
    edit  = B.sortByName ords $ T.getTermNames he
    ords  = (B.orderingCap . S.orderingTermName) <$> ns


-- ============================================  Portion of list

-- | Portion description.
data Portion = Portion
    { portionTop      :: Maybe Int   -- ^ Top /N/ elements.
    , portionMiddle   :: Maybe Int   -- ^ Middle /N/ elements.
    , portionBottom   :: Maybe Int   -- ^ Bottom /N/ elements.
    , portionParts    :: [Int]       -- ^ Indecies of /N/ chunks.
    , portionPer      :: Maybe Int   -- ^ Divided /N/ chunks.
    } deriving (Show, Eq, Ord)

instance B.Default Portion where
    def = Portion { portionTop     = Nothing
                  , portionMiddle  = Nothing
                  , portionBottom  = Nothing
                  , portionParts   = []
                  , portionPer     = Nothing }

-- | Test portion is not effective.
nullPortion :: O.Test Portion
nullPortion Portion { portionTop     = Nothing
                    , portionMiddle  = Nothing
                    , portionBottom  = Nothing
                    , portionPer     = Nothing } = True
nullPortion _ = False

-- | Take portion of list.
takePortion :: (Ord a) => Portion -> [a] -> [a]
takePortion p xs = xs' where
    xs' = B.unique (top p ++ mid p ++ bot p ++ parts p)

    top Portion { portionTop = Just n } = take n xs
    top _ = []

    mid Portion { portionMiddle = Just n } = B.takeMiddle n xs
    mid _ = []

    bot Portion { portionBottom = Just n } = B.takeEnd n xs
    bot _ = []

    parts Portion { portionParts = ps, portionPer = Just n } =
        concat $ B.takeChunks n ps xs
    parts _ = []


-- ============================================  Portion of relation

-- | Portion description for relation.
type RelPortion = ([S.TermName], Portion)

-- | Take effective (non-null) portion of relation body.
takeRelPortion :: (T.GetTermNames he, Ord c) => RelPortion -> he -> [[c]] -> [[c]]
takeRelPortion (ord, p) he body
    | nullPortion p  = body
    | otherwise      = takePortion p $ relBodyOrder ord he body

