{-# OPTIONS_GHC -Wall #-}

-- | General utilities

module Koshucode.Baala.Base.Prelude.Class
( Name (..),
  Named,
  named,
  Map,
  Pred,
  YesNo (..),
  Index,
  Choose (..),
) where

-- | Types that has name
class Name a where
    name  :: a -> String
    names :: [a] -> [String]
    names = map name

-- | Entry in association list.
type Named a = (String, a)

named :: (Name a) => a -> Named a
named a = (name a, a)

type Map a = a -> a

type Pred a = a -> Bool

data YesNo a = Yes a | No a
               deriving (Show, Eq, Ord)

type Index = Int

-- ----------------------  Choose

class Choose m where
    (<|>) :: m a -> m a -> m a

instance Choose Maybe where
    (Just a)  <|> _  =  Just a
    (Nothing) <|> b  =  b

instance Choose (Either a) where
    (Right a)  <|> _  =  Right a
    (Left _)   <|> b  =  b

