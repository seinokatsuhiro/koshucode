{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.Short
( Short (..),
  ShortDef,
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
import qualified Koshucode.Baala.Base.Token.Bracket   as B
import qualified Koshucode.Baala.Base.Token.TokenLine as B

data Short a =
    Short { shortSource :: [B.CodePoint]
          , shortHead   :: [ShortDef]
          , shortBody   :: a }
    deriving (Show, Ord, Eq)

type ShortDef = B.Named String

instance Functor Short where
    fmap f (Short pt he bo) = Short pt he $ f bo

shortMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
shortMap = map . fmap

shortM :: (Monad m) => Short (m a) -> m (Short a)
shortM (Short pt he bo) = return . Short pt he =<< bo

shortMapM :: (Monad m) => (a -> m b) -> [Short a] -> m [Short b]
shortMapM f = mapM $ shortM . fmap f

shortTrim :: B.Map [Short [a]]
shortTrim = B.omit $ null . shortBody

shortEmpty :: B.StringMap
shortEmpty = shortText []

sortWith :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortWith f = map snd . B.sort . map g where
    g x = (f x, x)

shortText :: [ShortDef] -> B.StringMap
shortText = loop . reverse . sortWith len where
    len = length . snd
    loop [] s | null s           = "<empty>"
              | B.isSimpleWord s = '\'' : s
              | otherwise        = B.bracketQuote s
    loop ((prefix, replace) : sh) s =
        case L.stripPrefix replace s of
          Just s2 -> prefix ++ "." ++ text2 s2
          _       -> loop sh s

    text2 s   | B.isSimpleWord s = s
              | otherwise        = B.bracketQuote s

