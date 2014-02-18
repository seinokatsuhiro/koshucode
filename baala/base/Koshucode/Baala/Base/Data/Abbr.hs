{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Abbr
( Abbr (..),
  abbrMap,
  abbrAb,
  abbrAbList,
  abbrTrim,
) where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Abort   as B

data Abbr a =
    Abbr { abbrHead :: [B.Named String]
         , abbrBody :: a }
    deriving (Show, Ord, Eq)

instance Functor Abbr where
    fmap f (Abbr a b) = Abbr a $ f b

abbrMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
abbrMap = map . fmap

abbrAb :: Abbr (B.Ab a) -> B.Ab (Abbr a)
abbrAb (Abbr a (Right x)) = Right $ Abbr a x
abbrAb (Abbr _ (Left  x)) = Left x

abbrAbList :: Abbr [B.Ab a] -> B.Ab (Abbr [a])
abbrAbList (Abbr a xs) = abbrAb $ Abbr a $ sequence xs

abbrTrim :: B.Map ([Abbr [a]])
abbrTrim = filter $ not . null . abbrBody

