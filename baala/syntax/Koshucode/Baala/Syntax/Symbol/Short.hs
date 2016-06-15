{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Short sign.

module Koshucode.Baala.Syntax.Symbol.Short
  ( -- * Data type
    Short (..), ShortDef,
  
    -- * Utility
    shortTrim, shortM, shortListM,
    shortGroup,
  
    -- * Shorten
    shortText,
  ) where

import qualified Data.List                                as L
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax.Symbol.AngleText  as S
import qualified Koshucode.Baala.Syntax.Symbol.Next       as S


-- ----------------------  Data type

-- | Something with short definition.
data Short a =
    Short { shortSource :: [B.CodePt]
          , shortHead   :: [ShortDef]
          , shortBody   :: a }
    deriving (Show, Ord, Eq)

-- | Short prefix and replacement.
type ShortDef = B.Named String

instance Functor Short where
    fmap f (Short pt he bo) = Short pt he $ f bo

shortTrim :: B.Map [Short [a]]
shortTrim = B.omit $ null . shortBody

shortM :: (Monad m) => Short (m a) -> m (Short a)
shortM (Short pt he bo) = return . Short pt he =<< bo

shortListM :: (Monad m) => [Short (m a)] -> m [Short a]
shortListM = mapM shortM

shortGroup :: [Short a] -> [Short [a]]
shortGroup [] = []
shortGroup (Short cp1 sh1 a : xs) =
    case shortGroup xs of
      Short _ sh2 as : xs' | sh1 == sh2  -> Short cp1 sh1 (a:as) : xs'
      []                                 -> [Short cp1 sh1 [a]]
      xs'                                -> Short cp1 sh1 [a] : xs'


-- ----------------------  Shorten

-- | String shortener.
shortText :: [ShortDef] -> B.Shorten
shortText = loop . reverse . B.sortWith len where
    len = length . snd

    loop [] _ = Nothing
    loop ((prefix, replace) : sh) s =
        case L.stripPrefix replace s of
          Just s2             -> Just $ prefix ++ "." ++ text2 s2
          _                   -> loop sh s

    text2 s   | isGeneralText s   = s
              | otherwise         = S.angleQuote s

-- | Test string is general sign.
isGeneralText :: B.Pred String
isGeneralText = all S.isGeneralChar

