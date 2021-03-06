{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Partial-order scale.

module Koshucode.Baala.Rop.Flat.Applied.PoScale
  ( PoScale, PoScaleCalc,
    poScale, poScaleHeight, poScaleDepth,
    poHeight, poDepth,
  ) where

import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Koshucode.Baala.DataPlus  as K

-- | Mapping from element to subelements and scale.
--   Scale of element is defined by
--   1 + the maximum scale of subelements.
--   If elements are cyclic, scales of these elements are -1.
type PoScale a = Map.Map a ([a], Maybe Int)

-- | Calculate partial-order scale.
type PoScaleCalc a = [(a, a)] -> [(a, Int)]

-- | Extract partial-order scale.
poScale :: PoScale a -> [(a, Int)]
poScale m = K.mapMaybe f $ Map.assocs m where
    f (x, (_, Just r))  = Just (x, r)
    f (_, (_, Nothing)) = Nothing

-- | Calculate upward partial-order scale.
--
--   >>> poScaleHeight [("a","b"), ("a","c"), ("b","d"), ("c","d")]
--   [("a",0), ("b",1), ("c",1), ("d",2)]
--
poScaleHeight :: (Ord a) => PoScaleCalc a
poScaleHeight = poScale . poHeight

-- | Calculate downward partial-order scale.
--
--   >>> poScaleDepth [("a","b"), ("a","c"), ("b","d"), ("c","d")]
--   [("a",2), ("b",1), ("c",1), ("d",0)]
--
poScaleDepth :: (Ord a) => PoScaleCalc a
poScaleDepth = poScale . poDepth

-- | Calculate partial-order height.
poHeight :: (Ord a) => [(a, a)] -> PoScale a
poHeight = poScaleBy K.gatherToMapSwap

-- | Calculate partial-order depth.
poDepth :: (Ord a) => [(a, a)] -> PoScale a
poDepth = poScaleBy K.gatherToMap

poScaleBy :: (Ord a) => ([(a, a)] -> Map.Map a [a]) -> [(a, a)] -> PoScale a
poScaleBy gather ord = poScaleUpdate m where
    m       = Map.map v $ gather ord
    v subs  = (subs, Nothing)

-- | Update partial-order scale for all elements.
poScaleUpdate :: (Ord a) => K.Map (PoScale a)
poScaleUpdate m = foldr poScaleUpdate1 m $ Map.keys m

-- | Update partial-order scale for an element.
poScaleUpdate1 :: forall a. (Ord a) => a -> K.Map (PoScale a)
poScaleUpdate1 x1 m1 = m2 where
    m2 = loop Set.empty x1 m1

    loop :: Set.Set a -> a -> K.Map (PoScale a)
    loop visit x m
        | Set.member x visit = Map.update neg x m
        | otherwise = case Map.lookup x m of
                        Nothing             -> Map.insert x zero m
                        Just ([], Nothing)  -> Map.update zerof x m
                        Just (sub, Nothing) -> let visit' = Set.insert x visit
                                               in scale x sub $ foldr (loop visit') m sub
                        Just (_,  Just _)   -> m

    zero           = p [] 0
    zerof _        = Just zero
    neg  (sub, _)  = Just $ p sub (-1)
    up r (sub, _)  = Just $ p sub (r + 1)
    p sub r        = (sub, Just r)

    scale :: a -> [a] -> K.Map (PoScale a)
    scale x sub m =
        case get m `mapM` sub of
          Nothing -> m
          Just rs | -1 `elem` rs -> Map.update neg x m
                  | otherwise    -> let ymax = maximum rs
                                    in Map.update (up ymax) x m

    get :: PoScale a -> a -> Maybe Int
    get m sub = case Map.lookup sub m of
                  Just (_, Just r) -> Just r
                  _                -> Nothing

-- poScale $ poHeight [("a","b"), ("a","c"), ("b","d"), ("c","d")]
-- poScale $ poDepth [("a","b"), ("a","c"), ("b","d"), ("c","d")]
-- poScale $ poHeight [("a","b"), ("b","a")]

