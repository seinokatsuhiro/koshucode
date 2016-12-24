{-# OPTIONS_GHC -Wall #-}

-- | Additional functions for pairs.

module Koshucode.Baala.Base.Prelude.Pair
  ( -- * Map
    mapFst, mapSnd,
    consFst, consSnd,
    mapFstTo,
    -- * Either
    right2, right3, right4,
    -- * Monad
    sequenceFst, sequenceSnd,
  ) where

import qualified Koshucode.Baala.Overture            as O


-- ----------------------  Map

-- | Apply function to 'fst' element.
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

-- | Apply function to 'snd' element.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

-- | Cons up an element to 'fst' list.
consFst :: a -> O.Map ([a], b)
consFst x = mapFst (x:)

-- | Cons up an element to 'snd' list.
consSnd :: b -> O.Map (a, [b])
consSnd x = mapSnd (x:)

-- | Apply function to 'fst' element in functor data.
--
--  >>> mapFstTo length $ Just ("aa", "bbbb")
--  Just (2, "bbbb")
--
mapFstTo :: (Functor m) => (a -> c) -> m (a, b) -> m (c, b)
mapFstTo = fmap . mapFst


-- ----------------------  Either

-- | Create right pair from either pair.
--
--   >>> right2 (Right 'a', Right 'b') :: Either Char (Char, Char)
--   Right ('a','b')
--
--   >>> right2 (Left 'a', Right 'b') :: Either Char (Char, Char)
--   Left 'a'
--
right2 :: (Either a b, Either a c) -> Either a (b, c)
right2 (Right b, Right c) = Right (b, c)
right2 (Left a, _) = Left a
right2 (_, Left a) = Left a

-- | Create right triple from either triple.
right3 :: (Either a b, Either a c, Either a d) -> Either a (b, c, d)
right3 (Right b, Right c, Right d) = Right (b, c, d)
right3 (Left a, _, _) = Left a
right3 (_, Left a, _) = Left a
right3 (_, _, Left a) = Left a

-- | Create right quadruple from either quadruple.
right4 :: (Either a b, Either a c, Either a d, Either a e) -> Either a (b, c, d, e)
right4 (Right b, Right c, Right d, Right e) = Right (b, c, d, e)
right4 (Left a, _, _, _) = Left a
right4 (_, Left a, _, _) = Left a
right4 (_, _, Left a, _) = Left a
right4 (_, _, _, Left a) = Left a


-- ----------------------  Monad

-- | Monadic sequencing of 'fst' elements.
sequenceFst :: (Monad m) => [(m a, b)] -> m [(a, b)]
sequenceFst xs =
    do let (fs, ss) = unzip xs
       fs' <- sequence fs
       return $ zip fs' ss

-- | Monadic sequencing of 'snd' elements.
--
--   >>> sequenceSnd [("a", Just "x"), ("b", Just "y")]
--   Just [("a","x"), ("b","y")]
--
--   >>> sequenceSnd [("a", Just "x"), ("b", Nothing)]
--   Nothing
--
sequenceSnd :: (Monad m) => [(a, m b)] -> m [(a, b)]
sequenceSnd xs =
    do let (fs, ss) = unzip xs
       ss' <- sequence ss
       return $ zip fs ss'
