{-# OPTIONS_GHC -Wall #-}

-- | String utility.

module Koshucode.Baala.Overture.Text.Utility
  ( -- * Trim
    trimLeft, trimRight, trimBoth,
    addSpace,
    -- * Padding
    padLeft, padRight, 
    padLeftWith, padRightWith,
    stringWidth,
    -- * Put
    putShow, putShowLn,
    putLines, hPutLines, hPutEmptyLine,
    -- * Read
    readInt, readInteger,
  ) where

import qualified Data.Char                      as Ch
import qualified System.IO                      as IO
import qualified Koshucode.Baala.Overture.Type  as O


-- ----------------------  Trim

isSpace :: Char -> Bool
isSpace c = Ch.isSpace c    -- UnicodeSeprator | UnicodeOther

-- | Remove space and space-like characters from the beginning of string.
trimLeft :: O.Map String
trimLeft = dropWhile isSpace

-- | Remove space and space-like characters from the end of string.
trimRight :: O.Map String
trimRight [] = []
trimRight (x : xs) =
    case x : trimRight xs of
      [y] | isSpace y -> []
      ys -> ys

-- | Remove space and space-like characters
--   from the beginning and end of string.
trimBoth :: O.Map String
trimBoth = trimRight . trimLeft

-- | Append space character if first character is non-space.
--
--   >>> addSpace "aaa"
--   " aaa"
--
--   >>> addSpace " bbb"
--   " bbb"
--
--   >>> addSpace ""
--   ""
--
addSpace :: O.Map String
addSpace cs@(c : _) | Ch.isSpace c  = cs
                    | otherwise     = ' ' : cs
addSpace ""                         = ""


-- ----------------------  Padding

-- | Add spaces to the left.
--
--   >>> padLeft 10 "abc"
--   "       abc"
--
padLeft :: Int -> O.Map String
padLeft = padLeftWith ' '

-- | Add spaces to the right.
--
--   >>> padRight 10 "abc"
--   "abc       "
--
padRight :: Int -> O.Map String
padRight = padRightWith ' '

-- | Add given character to the left.
padLeftWith :: Char -> Int -> O.Map String
padLeftWith p n s = replicate rest p ++ s where
    rest = max 0 (n - stringWidth s)

-- | Add given character to the right.
padRightWith :: Char -> Int -> O.Map String
padRightWith p n s = s ++ replicate rest p where
    rest = max 0 (n - stringWidth s)

-- | Calculate width of string.
stringWidth :: String -> Int
stringWidth = sum . map charWidth

-- | Character width.
charWidth :: Char -> Int
charWidth c
    | Ch.ord c >= 256  = 2
    | otherwise        = 1


-- ----------------------  Put

-- | Print showing value.
putShow :: (Show a) => a -> IO ()
putShow = putStr . show

-- | Print showing value with newline.
putShowLn :: (Show a) => a -> IO ()
putShowLn = putStrLn . show

-- | Print multiple lines.
putLines :: [String] -> IO ()
putLines = putStr . unlines

-- | Print multiple lines.
hPutLines :: IO.Handle -> [String] -> IO ()
hPutLines h = (IO.hPutStrLn h `mapM_`)

-- | Print empty line.
hPutEmptyLine :: IO.Handle -> IO ()
hPutEmptyLine h = IO.hPutStrLn h ""


-- ----------------------  Read

readJust :: (Read a) => String -> Maybe a
readJust s = case reads s of
              [(x, "")] -> Just x
              _         -> Nothing

-- | Read integer.
--
--   >>> readInt "12"
--   Just 12
--
--   >>> readInt "12.3"
--   Nothing
--
--   >>> readInt "12345678901234567890"
--   Just (-6101065172474983726)
--
readInt :: String -> Maybe Int
readInt = readJust

-- | Read integer.
--
--   >>> readInteger "12345678901234567890"
--   Just 12345678901234567890
--
readInteger :: String -> Maybe Integer
readInteger = readJust

