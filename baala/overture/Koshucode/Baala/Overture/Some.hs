{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Something exists somewhere.

module Koshucode.Baala.Overture.Some
 ( Some (..),
   (|?|),
   orElse, (<||>),
 ) where

-- | Something exists.
class Some s where
    -- | Test something exists.
    --
    --   >>> some $ Just "foo"
    --   True
    --
    some :: s a -> Bool
    some = not . none

    -- | Test something does not exists.
    --
    --   >>> none ["foo"]
    --   False
    --
    none :: s a -> Bool
    none = not . some

    -- | Extract something.
    thing :: s a -> Maybe a

-- | Just.
instance Some Maybe where
    some (Just _)   = True
    some (Nothing)  = False

    thing = id

-- | Right.
instance Some (Either a) where
    some (Right _)  = True
    some (Left  _)  = False

    thing (Right a) = Just a
    thing (Left  _) = Nothing

-- | Non-empty list.
instance Some [] where
    none = null

    thing (a : _) = Just a
    thing []      = Nothing

-- | Extract thing or return default thing.
--
--   >>> ["foo", "bar"] |?| "baz"
--   "foo"
--
--   >>> [] |?| "baz"
--   "baz"
--
(|?|) :: (Some s) => s a -> a -> a
(|?|) s a0 = case thing s of
               Just a  -> a
               Nothing -> a0

-- Same as ||
infixr 2 <||>
infixr 2 `orElse`

-- | Calculate alternatives if prior expression failed.
orElse :: (Some s) => s a -> s a -> s a
orElse a b | some a     = a
           | otherwise  = b

-- | Infix operator for 'orElse'.
--
--   >>> Nothing <||> Just "foo" <||> Just "bar"
--  Just "foo"
--
(<||>) :: (Some s) => s a -> s a -> s a
(<||>) = orElse

