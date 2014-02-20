{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Short
( Short (..),
  shortMap,
  shortAb,
  shortAbList,
  shortTrim,
) where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Abort   as B

data Short a =
    Short { shortHead :: [B.Named String]
          , shortBody :: a }
    deriving (Show, Ord, Eq)

instance Functor Short where
    fmap f (Short a b) = Short a $ f b

shortMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
shortMap = map . fmap

shortAb :: Short (B.Ab a) -> B.Ab (Short a)
shortAb (Short a (Right x)) = Right $ Short a x
shortAb (Short _ (Left  x)) = Left x

shortAbList :: Short [B.Ab a] -> B.Ab (Short [a])
shortAbList (Short a xs) = shortAb $ Short a $ sequence xs

shortTrim :: B.Map ([Short [a]])
shortTrim = filter $ not . null . shortBody

