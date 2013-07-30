{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

-- | General utilities

module Koshucode.Baala.Base.Prelude.Class
( Name (..),
  Named,
  Map,
  Listmap,
  YesNo (..),
) where



{-| Entry in association list. -}
type Named a = (String, a)

-- | Types that has name
class Name a where
    name :: a -> String
    names :: [a] -> [String]
    names = map name

type Map a = a -> a

type Listmap a = forall b. Map [b]

data YesNo a = Yes a | No a
               deriving (Show, Eq, Ord)

