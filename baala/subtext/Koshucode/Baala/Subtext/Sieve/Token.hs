{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token for sieve pattern. 

module Koshucode.Baala.Subtext.Sieve.Token
  ( SivToken (..),
    SivBracket (..),
    SivKey (..),
    sivTokens,
  ) where

import qualified Data.Char                    as Ch
import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B

-- | Sieve token.
data SivToken t
    = SivText  t
    | SivKey   SivKey
    | SivOpen  SivBracket
    | SivClose SivBracket
      deriving (Show, Eq, Ord)

instance B.GetCodePos (SivToken t) where
    getCP  _ = B.def
    getCPs _ = []

-- | Sieve bracket.
data SivBracket
    = SivGroup
    | SivRepeat Int
    | SivOption
      deriving (Show, Eq, Ord)

sivBracket :: String -> Maybe (SivToken t)
sivBracket "("   = Just $ SivOpen    SivGroup
sivBracket ")"   = Just $ SivClose   SivGroup
sivBracket "{"   = Just $ SivOpen  $ SivRepeat 0
sivBracket "}"   = Just $ SivClose $ SivRepeat 0
sivBracket "{-"  = Just $ SivOpen  $ SivRepeat 1
sivBracket "-}"  = Just $ SivClose $ SivRepeat 1
sivBracket "["   = Just $ SivOpen    SivOption
sivBracket "]"   = Just $ SivClose   SivOption
sivBracket _     = Nothing

tBracket :: (O.Textual t) => t -> Maybe (SivToken t, t)
tBracket t = bracket2 t O.<||> bracket1 t where
    bracket2 (O.tCut2 -> O.Jp2 a b t') = pair t' <$> sivBracket [a,b]
    bracket2 _ = Nothing

    bracket1 (O.tCut -> O.Jp a t') = pair t' <$> sivBracket [a]
    bracket1 _ = Nothing

    pair t' b = (b, t')

-- | Sieve keyword.
data SivKey
    = SivAnyChar   -- ^ __[ _ ]__ Any single character.
    | SivAnyText   -- ^ __[ * ]__ Arbitrary-length any text.
    | SivRange     -- ^ __[ - ]__ Character between two characters.
      deriving (Show, Eq, Ord)

sivKey :: (O.Textual t) => Char -> t -> Maybe (SivKey, t)
sivKey '*' t = Just (SivAnyText, t)
sivKey '_' t = Just (SivAnyChar, t)
sivKey '-' t = Just (SivRange, t)
sivKey '.' (O.tCut2 -> O.Jp2 '.' '.' t) = Just (SivAnyText, t)
sivKey _ _ = Nothing

-- | Convert textual value to sieve token list.
--
--  >>> print O.<#!> sivTokens "foo{ba_}[0-9]baz_"
--  SivText "foo"
--  SivOpen (SivRepeat 0)
--  SivText "ba"
--  SivKey SivAnyChar
--  SivClose (SivRepeat 0)
--  SivOpen SivOption
--  SivText "0"
--  SivKey SivRange
--  SivText "9"
--  SivClose SivOption
--  SivText "baz_"
--
sivTokens :: (O.Textual t) => t -> [SivToken t]
sivTokens = toks 0

toks :: (O.Textual t) => Int -> t -> [SivToken t]
toks lv (tBracket -> Just (b, t))  = b : toks (lv + level b) t
toks lv t | O.tIsEmpty t           = []
          | otherwise              = text lv t 0 t

text :: (O.Textual t) => Int -> t -> Int -> t -> [SivToken t]
text lv t0 = loop where
    loop n (tBracket -> Just (b, t)) = push n $ b : toks (lv + level b) t
    loop n (O.tCut -> O.Jp c t)
      | lv == 0       = loop (n + 1) t
      | Ch.isSpace c  = push n $ toks lv t
      | otherwise     = case sivKey c t of
                          Nothing -> loop (n + 1) t
                          Just (k, t') -> push n $ (SivKey k) : toks lv t'
    loop n _ = push n []

    push n = case O.tTake n t0 of
               t | O.tIsEmpty t  -> id
                 | otherwise     -> (SivText t :)

level :: SivToken t -> Int
level (SivOpen  _)  =  1
level (SivClose _)  = -1
level _             =  0

