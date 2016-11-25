{-# OPTIONS_GHC -Wall #-}

-- | General utilities.

module Koshucode.Baala.Base.Prelude.Class
  ( Name (..),
    Named,
    named,
    Index,
    Collect,
    Choose (..),
  ) where

-- | Types that has name.
class Name a where
    name  :: a -> String
    names :: [a] -> [String]
    names = map name

-- | Entry in association list.
type Named a = (String, a)

-- | Create named pair.
named :: (Name a) => a -> Named a
named a = (name a, a)

-- | Numerical index.
type Index = Int

-- | Wrap list.
type Collect a = [a] -> a


-- ----------------------  Choose

-- | Choose one of two.
class Choose m where
    (<|>) :: m a -> m a -> m a

instance Choose Maybe where
    (Just a)  <|> _  = Just a
    (Nothing) <|> b  = b

instance Choose (Either a) where
    (Right a)  <|> _  = Right a
    (Left _)   <|> b  = b

