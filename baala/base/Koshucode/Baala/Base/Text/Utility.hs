{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Utility
( trimLeft, trimRight, trimBoth,
) where

import qualified Data.Char                     as Ch
import qualified Koshucode.Baala.Base.Prelude  as B

isSpace :: Char -> Bool
isSpace c = Ch.isSpace c    -- UnicodeSeprator | UnicodeOther

trimLeft :: B.Map String
trimLeft = dropWhile isSpace

trimRight :: B.Map String
trimRight [] = []
trimRight (x : xs) =
    case x : trimRight xs of
      [y] | isSpace y -> []
      ys -> ys

trimBoth :: B.Map String
trimBoth = trimRight . trimLeft
