{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Something exists somewhere.

module Koshucode.Baala.Overture.Some
 ( Some (..), orElse, (<||>),
 ) where

-- | Something exists.
class Some a where
    -- | Test something exists.
    --
    --   >>> some $ Just "foo"
    --   True
    --
    some :: a -> Bool
    some = not . none

    -- | Test something does not exists.
    --
    --   >>> none ["foo"]
    --   False
    --
    none :: a -> Bool
    none = not . some

-- | True.
instance Some Bool where
    some = id

-- | Just.
instance Some (Maybe a) where
    some (Just _)   = True
    some (Nothing)  = False

-- | Right.
instance Some (Either a b) where
    some (Right _)  = True
    some (Left  _)  = False

-- | Non-empty list.
instance Some [a] where
    none = null

-- Same as ||
infixr 2 <||>
infixr 2 `orElse`

-- | Calculate alternatives if prior expression failed.
orElse :: (Some a) => a -> a -> a
orElse a b | some a     = a
           | otherwise  = b

-- | Infix operator for 'orElse'.
--
--   >>> Nothing <||> Just "foo" <||> Just "bar"
--  Just "foo"
--
(<||>) :: (Some a) => a -> a -> a
(<||>) = orElse

