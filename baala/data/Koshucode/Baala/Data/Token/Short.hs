{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Short sign.

module Koshucode.Baala.Data.Token.Short
  ( -- * Data type
    Short (..), ShortDef,
  
    -- * Utility
    shortTrim, shortM, shortListM,
    shortGroup,
  
    -- * Shortener
    shortText,
    isGeneralText, isGeneralChar,
    isOrdinaryChar,
  ) where

import qualified Data.List                             as L
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Data.Token.AngleText  as D


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


-- ----------------------  Shortener

-- | String shortener.
shortText :: [ShortDef] -> B.Shortener
shortText = loop . reverse . B.sortWith len where
    len = length . snd

    loop [] _ = Nothing
    loop ((prefix, replace) : sh) s =
        case L.stripPrefix replace s of
          Just s2             -> Just $ prefix ++ "." ++ text2 s2
          _                   -> loop sh s

    text2 s   | isGeneralText s   = s
              | otherwise         = D.angleQuote s

-- | Test string is general sign.
isGeneralText :: B.Pred String
isGeneralText = all isGeneralChar

-- | Test char is general.
isGeneralChar :: B.Pred Char
isGeneralChar c =
    case B.majorGeneralCategory c of
      B.UnicodeLetter       -> True
      B.UnicodeMark         -> True
      B.UnicodeNumber       -> True
      B.UnicodeSymbol       -> c `elem` "+<=>~"
      B.UnicodePunctuation  -> c `elem` "-_.*#"
      _                     -> False

-- | Test char is ordinary.
isOrdinaryChar :: B.Pred Char
isOrdinaryChar c =
    case B.majorGeneralCategory c of
      B.UnicodeLetter       -> True
      B.UnicodeMark         -> True
      B.UnicodeNumber       -> True
      _                     -> c `elem` "-_"

