{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Pair
( mapFst, mapSnd,
  cons1,
  mapFstTo, mapSndTo,
  maybePairs,
  sequenceFst, sequenceSnd,
) where

import qualified Control.Applicative as A

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

cons1 :: a -> ([a], b) -> ([a], b)
cons1 x = mapFst (x:)

mapFstTo :: (Functor m) => (a -> c) -> m (a, b) -> m (c, b)
mapFstTo = fmap . mapFst

mapSndTo :: (Functor m) => (b -> c) -> m (a, b) -> m  (a, c)
mapSndTo = fmap . fmap

maybePairs :: [a] -> Maybe [(a, a)]
maybePairs (a:b:xs) = A.liftA ((a, b):) $ maybePairs xs
maybePairs []       = Just []
maybePairs _        = Nothing

sequenceFst :: (Monad m) => [(m a, b)] -> m [(a, b)]
sequenceFst xs =
    do let (fs, ss) = unzip xs
       fs' <- sequence fs
       return $ zip fs' ss

sequenceSnd :: (Monad m) => [(a, m b)] -> m [(a, b)]
sequenceSnd xs =
    do let (fs, ss) = unzip xs
       ss' <- sequence ss
       return $ zip fs ss'
