{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Utility
  ( -- * Trim
    trimLeft, trimRight, trimBoth,
    -- * Padding
    padRight, padLeft, stringWidth,
    -- * Put
    putShow, putShowLn, putLines,
    -- * Read
    readInt, readInteger,
  ) where

import qualified Data.Char                     as Ch
import qualified Koshucode.Baala.Base.Prelude  as B


-- ----------------------  Trim

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


-- ----------------------  Padding

-- | Add spaces to right.
--
--   >>> padRight 10 "abc"
--   "abc       "
padRight :: Int -> B.Map String
padRight n s = s ++ replicate rest ' ' where
    rest = max 0 (n - stringWidth s)

-- | Add spaces to left.
--
--   >>> padLeft 10 "abc"
--   "       abc"
padLeft :: Int -> B.Map String
padLeft n s = replicate rest ' ' ++ s where
    rest = max 0 (n - stringWidth s)

stringWidth :: String -> Int
stringWidth = sum . map charWidth

charWidth :: Char -> Int
charWidth c
    | Ch.ord c >= 256  = 2
    | otherwise        = 1


-- ----------------------  Put

putShow :: (Show a) => a -> IO ()
putShow = putStr . show

putShowLn :: (Show a) => a -> IO ()
putShowLn = putStrLn . show

putLines :: [String] -> IO ()
putLines = putStr . unlines


-- ----------------------  Read

readJust :: (Read a) => String -> Maybe a
readJust s = case reads s of
              [(x, "")] -> Just x
              _         -> Nothing

readInt :: String -> Maybe Int
readInt = readJust

readInteger :: String -> Maybe Integer
readInteger = readJust

