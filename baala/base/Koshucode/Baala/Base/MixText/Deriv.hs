{-# OPTIONS_GHC -Wall #-}

-- | Derived mix text.

module Koshucode.Baala.Base.MixText.Deriv
  ( mixTab,
    mixTerms,
    mixJudge,
  ) where

import qualified Koshucode.Baala.Overture                     as O
import qualified Koshucode.Baala.Base.MixText.MixText         as B
import qualified Koshucode.Baala.Base.MixText.MixClass        as B

-- | Mix text tab character.
--
--   >>> B.mixString "a" `mixTab` B.mixString "b"
--   MixText "a\tb"
--
mixTab :: B.MixText
mixTab = B.mixChar '\t'

-- | Join mix text list by separetor.
--
--   >>> mixJoin B.mix1 ["foo", "bar", "baz"]
--   MixText "foo bar baz"
--
--   >>> mixJoin mixTab ["foo", "bar", "baz"]
--   MixText "foo\tbar\tbaz\t"
--
mixJoin :: (B.Mix a, B.Mix b) => a -> [b] -> B.MixText
mixJoin sep = loop where
    loop [] = B.mixEmpty
    loop (m:ms) = B.mix m O.++ B.mix sep O.++ loop ms

-- | Concatenate terms of content.
--
--  >>> mixTerms B.mix2 [("a", "foo"), ("b", "bar")]
--  MixText "/a foo  /b bar"
--
{-# DEPRECATED mixTerms "Use 'termsToMix2' instead." #-}
mixTerms :: (B.Mix sep,  B.Mix c) => sep -> [(String, c)] -> B.MixText
mixTerms sep ts = mixJoin sep (f <$> ts) where
    f (n, c) = mixTermName n `B.mixSep` B.mix c

mixTermName :: String -> B.MixText
mixTermName n = B.mixString ('/' : n)

-- | Create judgement using mix text.
--
--  >>> mixJudge B.mix2 "AB" [("a", "foo"), ("b", "bar")]
--  MixText "|-- AB  /a foo  /b bar"
--
{-# DEPRECATED mixJudge "Use 'judgeMix2' instead." #-}
mixJudge :: (B.Mix sep, B.Mix cl, B.Mix c) => sep -> cl -> [(String, c)] -> B.MixText
mixJudge sep cl ts = B.mixString "|-- " O.++ B.mix cl O.++ B.mix sep O.++ mixTerms sep ts

