{-# OPTIONS_GHC -Wall #-}

-- | Infix operators.

module Koshucode.Baala.Overture.Infix
 ( -- * Infix operators
   (&), (++),

   -- ** Map
   (/$/), (<++>),
   (<?>), (<$$>),

   -- ** Monad
   (#$), (<#>), (<#!>),
   (<#.>), (<#++>),
 ) where

import Prelude hiding ((++))
import qualified Control.Monad
import qualified Data.Maybe                    as May
import qualified Koshucode.Baala.Overture.Type as O

infixr 0 &

-- | Pairing operator.
--
--   >>> "a" & "b"
--   ("a", "b")
--
--   >>> "a" & "b" & "c"
--   ("a", ("b", "c"))
--
{-# INLINE (&) #-}
(&) :: a -> b -> (a, b)
a & b = (a, b)

infixr 6 ++

-- | Associative operator of monoid,
--   same as 'mappend' or infix '<>'.
--
--   >>> Just "abc" ++ Nothing ++ Just "def"
--   Just "abcdef"
--
{-# INLINE (++) #-}
(++) :: (Monoid a) => a -> a -> a
(++) = mappend


-- ----------------------  Map

-- | Apply function to reversed list, and reverse back.
--
--   >>> take 3 $ "abcdefg"
--   "abc"
--
--   >>> take 3 /$/ "abcdefg"
--   "efg"
--
(/$/) :: O.Map [a] -> O.Map [a]
(/$/) f = reverse . f . reverse

-- | Same as 'concatMap'.
--
--   >>> words <++> ["foo bar", "baz quux"]
--   ["foo","bar","baz","quux"]
--
{-# INLINE (<++>) #-}
(<++>) :: (a -> [b]) -> [a] -> [b]
(<++>) = concatMap

-- | Same as 'May.mapMaybe'.
(<?>) :: (a -> Maybe b) -> [a] -> [b]
(<?>) = May.mapMaybe

infixl 4 <$$>

-- | Double fmap.
--
--   >>> length <$$> [("a", "apple"), ("b", "banana"), ("c", "cocoa")]
--   [("a",5), ("b",6), ("c",5)]
--
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x


-- ----------------------  Monad

infixr 1 #$

-- | Monadic application, same as '=<<'.
(#$) :: (Monad m) => (a -> m b) -> m a -> m b
(#$) = (=<<)

-- | Monadic mapping, same as 'mapM'.
{-# INLINE (<#>) #-}
(<#>) :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
(<#>) = mapM

-- | Execution by monadic mapping, same as 'mapM_'.
{-# INLINE (<#!>) #-}
(<#!>) :: (Monad m, Traversable t) => (a -> m b) -> t a -> m ()
(<#!>) = mapM_

infixr 1 <#.>

-- | Composition of monadic functions.
(<#.>) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<#.>) = (Control.Monad.<=<)

-- | Monadic concat mappping.
(<#++>) :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
(<#++>) f a = return . concat =<< (f <#> a)
