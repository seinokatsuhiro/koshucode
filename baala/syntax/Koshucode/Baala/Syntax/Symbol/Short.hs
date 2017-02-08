{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Short sign.

module Koshucode.Baala.Syntax.Symbol.Short
  ( -- * Data type
    Short (..), ShortDef,
  
    -- * Utility
    shortTrim, --shortM,
    shortListM,
    shortGroup,
  
    -- * Shorten
    shortText,
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax.Symbol.AngleText  as S
import qualified Koshucode.Baala.Syntax.Symbol.Symbol     as S


-- ----------------------  Data type

-- | Something with short definition.
data Short t a =
    Short { shortSource :: [B.TCodePos t]  -- ^ Source information
          , shortHead   :: [ShortDef t]   -- ^ Definition of short signs
          , shortBody   :: a            -- ^ Body with short signs
          } deriving (Show, Ord, Eq)

-- | Short prefix and replacement.
type ShortDef t = (t, t)

instance Functor (Short t) where
    fmap f (Short pt he bo) = Short pt he $ f bo

-- | Omit empty body.
shortTrim :: O.Map [Short t [a]]
shortTrim = B.omit $ null . shortBody

-- | Evaluate body of short structure.
shortM :: (Monad m) => Short t (m a) -> m (Short t a)
shortM (Short pt he bo) = return . Short pt he O.# bo

-- | Evaluate body of short structures.
shortListM :: (Monad m) => [Short t (m a)] -> m [Short t a]
shortListM = mapM shortM

-- | Group short structures.
shortGroup :: (Eq t) => [Short t a] -> [Short t [a]]
shortGroup [] = []
shortGroup (Short cp1 sh1 a : xs) =
    case shortGroup xs of
      Short _ sh2 as : xs' | sh1 == sh2  -> Short cp1 sh1 (a:as) : xs'
      []                                 -> [Short cp1 sh1 [a]]
      xs'                                -> Short cp1 sh1 [a] : xs'


-- ----------------------  Shorten

-- | String shortener.
--
--   >>> shortText [("f", "foo/bar/")] "foo/bar/baz"
--   Just "f.baz"
--
--   >>> shortText [("f", "foo/bar/")] "foo/baz"
--   Nothing
--
shortText :: forall t. (O.Textual t) => [ShortDef t] -> B.TransText t
shortText = loop . reverse . B.sortWith len where
    len :: ShortDef t -> Int
    len = O.tLength . snd

    loop :: [ShortDef t] -> B.TransText t
    loop [] _ = Nothing
    loop ((prefix, replace) : sh) s =
        case O.tDropPrefix replace s of
          Just s2   -> Just $ prefix O.++ ('.' O.<:> text2 s2)
          _         -> loop sh s

    text2 s | isGeneralText s  = s
            | otherwise        = S.angleQuote s

-- | Test text value is general sign.
isGeneralText :: (O.Textual t) => O.Test t
isGeneralText = O.tAll S.isGeneralChar

