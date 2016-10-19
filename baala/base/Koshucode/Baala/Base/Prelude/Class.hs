{-# OPTIONS_GHC -Wall #-}

-- | General utilities.

module Koshucode.Baala.Base.Prelude.Class
  ( Name (..),
    Named,
    named,
    Map, ManyMap,
    YesNo (..),
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

named :: (Name a) => a -> Named a
named a = (name a, a)

-- | Map from something to same type.
{-# DEPRECATED Map "Replaced by 'Koshucode.Baala.Overture.Type.Map'." #-}
type Map a = a -> a

-- | Map from something to list of something.
{-# DEPRECATED ManyMap "Replaced by 'Koshucode.Baala.Overture.Type.ManyMap'." #-}
type ManyMap a = a -> [a]

-- | Type of value which is classified as yes or no.
data YesNo a = Yes a | No a
               deriving (Show, Eq, Ord)

type Index = Int

type Collect a = [a] -> a

-- ----------------------  Choose

class Choose m where
    (<|>) :: m a -> m a -> m a

instance Choose Maybe where
    (Just a)  <|> _  =  Just a
    (Nothing) <|> b  =  b

instance Choose (Either a) where
    (Right a)  <|> _  =  Right a
    (Left _)   <|> b  =  b

