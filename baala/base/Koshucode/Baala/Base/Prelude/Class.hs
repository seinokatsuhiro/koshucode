{-# OPTIONS_GHC -Wall #-}

-- | General utilities

module Koshucode.Baala.Base.Prelude.Class
( Name (..),
  Named,
  named,
  Map,
  Pred,
  YesNo (..),
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

