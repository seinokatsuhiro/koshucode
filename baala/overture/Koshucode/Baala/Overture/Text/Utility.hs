{-# OPTIONS_GHC -Wall #-}

-- | String utility.

module Koshucode.Baala.Overture.Text.Utility
  ( -- * Trim
    trimBegin, trimEnd, trimBoth,
    -- * Padding
    padBegin, padEnd, 
    padBeginWith, padEndWith,
    stringWidth,
    addSpace,
    -- * Put
    putShow, putShowLn,
    putLines, hPutLines, hPutEmptyLine,
    -- * Read
    readInt, readInteger,
    stringHex, stringHexInt, stringHexInteger,
    intLowerHexString, intUpperHexString,
  ) where

import qualified Data.Char                      as Ch
import qualified System.IO                      as IO
import qualified Numeric                        as Num
import qualified Koshucode.Baala.Overture.Type  as O


-- ----------------------  Trim

isSpace :: Char -> Bool
isSpace c = Ch.isSpace c    -- UnicodeSeprator | UnicodeOther

-- | Remove space and space-like characters from the beginning of string.
--
--   >>> trimBegin "  abc  "
--   "abc  "
--
trimBegin :: O.StringMap
trimBegin = dropWhile isSpace

-- | Remove space and space-like characters from the end of string.
--
--   >>> trimEnd "  abc  "
--   "  abc"
--
trimEnd :: O.StringMap
trimEnd [] = []
trimEnd (x : xs) =
    case x : trimEnd xs of
      [y] | isSpace y -> []
      ys -> ys

-- | Remove space and space-like characters
--   from the beginning and end of string.
--
--   >>> trimBoth "  abc  "
--   "abc"
--
trimBoth :: O.StringMap
trimBoth = trimEnd . trimBegin


-- ----------------------  Padding

-- | Add spaces to the left.
--
--   >>> padBegin 10 "abc"
--   "       abc"
--
padBegin :: Int -> O.StringMap
padBegin = padBeginWith ' '

-- | Add spaces to the right.
--
--   >>> padEnd 10 "abc"
--   "abc       "
--
padEnd :: Int -> O.StringMap
padEnd = padEndWith ' '

-- | Add given character to the left.
--
--   >>> padBeginWith '.' 10 "abc"
--   ".......abc"
--
padBeginWith :: Char -> Int -> O.StringMap
padBeginWith p n s = replicate rest p ++ s where
    rest = max 0 (n - stringWidth s)

-- | Add given character to the right.
padEndWith :: Char -> Int -> O.StringMap
padEndWith p n s = s ++ replicate rest p where
    rest = max 0 (n - stringWidth s)

-- | Calculate width of string.
stringWidth :: String -> Int
stringWidth = sum . map charWidth

-- | Character width.
charWidth :: Char -> Int
charWidth c
    | Ch.ord c >= 256  = 2
    | otherwise        = 1

-- | Add space character if first character is non-space.
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
addSpace :: O.StringMap
addSpace cs@(c : _) | Ch.isSpace c  = cs
                    | otherwise     = ' ' : cs
addSpace ""                         = ""


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

readMaybe :: (String -> [(a, String)]) -> String -> Maybe a
readMaybe f s = case f s of
                  [(x, "")] -> Just x
                  _         -> Nothing

readDec :: (Eq n, Num n) => String -> Maybe n
readDec = readMaybe Num.readDec

-- | Read decimal integer.
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
readInt = readDec

-- | Read decimal integer.
--
--   >>> readInteger "12345678901234567890"
--   Just 12345678901234567890
--
readInteger :: String -> Maybe Integer
readInteger = readDec

-- | Read hexadecimal digits.
stringHex :: (Eq n, Num n) => String -> Maybe n
stringHex = readMaybe Num.readHex

-- | Read hexadecimal digits as 'Int'.
--
--   >>> stringHexInt "0F"
--   Just 15
--
stringHexInt :: String -> Maybe Int
stringHexInt = stringHex

-- | Read hexadecimal digits as 'Integer'.
--
--   >>> stringHexInteger "0F"
--   Just 15
--
stringHexInteger :: String -> Maybe Integer
stringHexInteger = stringHex

integralLowerHexString :: (Integral n, Show n) => n -> String
integralLowerHexString n = Num.showHex n ""

integralUpperHexString :: (Integral n, Show n) => n -> String
integralUpperHexString = map Ch.toUpper . integralLowerHexString

-- | Convert integer to hexadecimal string.
--
--   >>> intHexString 15
--   "f"
--
intLowerHexString :: Int -> String
intLowerHexString = integralLowerHexString

-- | Convert integer to hexadecimal string.
--
--   >>> intUpperHexString 15
--   "F"
--
intUpperHexString :: Int -> String
intUpperHexString = integralUpperHexString
