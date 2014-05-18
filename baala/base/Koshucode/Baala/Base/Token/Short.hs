{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.Short
( Short (..),
  shortMap,
  shortMapM,
  shortM,
  shortTrim,
) where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B

data Short a =
    Short { shortHead :: [B.ShortDef]
          , shortBody :: a }
    deriving (Show, Ord, Eq)

instance Functor Short where
    fmap f (Short a b) = Short a $ f b

shortMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
shortMap = map . fmap

shortM :: (Monad m) => Short (m a) -> m (Short a)
shortM (Short he bo) = return . Short he =<< bo

shortMapM :: (Monad m) => (a -> m b) -> [Short a] -> m [Short b]
shortMapM f = mapM $ shortM . fmap f

shortTrim :: B.Map [Short [a]]
shortTrim = filter $ not . null . shortBody
