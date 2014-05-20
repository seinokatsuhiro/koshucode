{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.Short
( Short (..),
  shortMap,
  shortMapM,
  shortM,
  shortTrim,

  shortEmpty,
  shortText,
) where

import qualified Data.List                            as L
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Token.HashWord  as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B

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
shortTrim = B.omit $ null . shortBody

shortEmpty :: B.StringMap
shortEmpty = shortText []

shortText :: [B.ShortDef] -> B.StringMap
shortText = loop where
    loop [] s | null s           = "#empty"
              | B.isSimpleWord s = '\'' : s
              | otherwise        = B.hashWord s
    loop ((prefix, long) : sh) s =
        case L.stripPrefix long s of
          Just s2 -> prefix ++ "." ++ text2 s2
          _       -> loop sh s

    text2 s   | B.isSimpleWord s = s
              | otherwise        = B.hashWord s

