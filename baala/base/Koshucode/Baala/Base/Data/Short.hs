{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Short
( ShortDef,
  Short (..),
  shortMap,
  shortMapM,
  shortM,
  shortTrim,
) where

import qualified Koshucode.Baala.Base.Prelude as B

type ShortDef = B.Named String

data Short a =
    Short { shortHead :: [ShortDef]
          , shortBody :: a }
    deriving (Show, Ord, Eq)

instance Functor Short where
    fmap f (Short a b) = Short a $ f b

shortMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
shortMap = map . fmap

shortMapM :: (Monad m) => (a -> m b) -> [Short a] -> m [Short b]
shortMapM f = mapM (shortM . fmap f)

shortM :: (Monad m) => Short (m a) -> m (Short a)
shortM (Short he bo) = return . Short he =<< bo

shortTrim :: B.Map [Short [a]]
shortTrim = filter $ not . null . shortBody

