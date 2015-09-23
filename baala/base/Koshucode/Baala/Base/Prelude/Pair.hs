{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Pair
  ( -- * Map
    mapFst, mapSnd,
    consFst, consSnd,
    mapFstTo, mapSndTo,
    -- * Either
    right2, right3, right4,
    -- * Monad
    sequenceFst, sequenceSnd,
  ) where

import qualified Koshucode.Baala.Base.Prelude.Class as B


-- ----------------------  Map

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

consFst :: a -> B.Map ([a], b)
consFst x = mapFst (x:)

consSnd :: b -> B.Map (a, [b])
consSnd x = mapSnd (x:)

mapFstTo :: (Functor m) => (a -> c) -> m (a, b) -> m (c, b)
mapFstTo = fmap . mapFst

mapSndTo :: (Functor m) => (b -> c) -> m (a, b) -> m  (a, c)
mapSndTo = fmap . fmap


-- ----------------------  Either

right2 :: (Either a b, Either a c) -> Either a (b, c)
right2 (Right b, Right c) = Right (b, c)
right2 (Left a, _) = Left a
right2 (_, Left a) = Left a

right3 :: (Either a b, Either a c, Either a d) -> Either a (b, c, d)
right3 (Right b, Right c, Right d) = Right (b, c, d)
right3 (Left a, _, _) = Left a
right3 (_, Left a, _) = Left a
right3 (_, _, Left a) = Left a

right4 :: (Either a b, Either a c, Either a d, Either a e) -> Either a (b, c, d, e)
right4 (Right b, Right c, Right d, Right e) = Right (b, c, d, e)
right4 (Left a, _, _, _) = Left a
right4 (_, Left a, _, _) = Left a
right4 (_, _, Left a, _) = Left a
right4 (_, _, _, Left a) = Left a


-- ----------------------  Monad

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
