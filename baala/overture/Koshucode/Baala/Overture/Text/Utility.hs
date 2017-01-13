{-# OPTIONS_GHC -Wall #-}

-- | Text utility.

module Koshucode.Baala.Overture.Text.Utility
  ( -- * Trim
    trimBegin, trimEnd, trimBoth,

    -- * Padding
    padBegin, padEnd, 
    padBeginWith, padEndWith,
    stringWidth,
    addSpace,

    -- * Code point
    isAsciiCode,
    isNewlineCode,
    integralHT,
    integralLF,
    integralVT,
    integralFF,
    integralCR,
    integralSpace,

    -- * Put
    putLn, hPutLn,
    putShow, putShowLn,
    putLines, hPutLines, 
    prompt,
  ) where

import qualified Data.Char                      as Ch
import qualified System.IO                      as IO
import qualified Koshucode.Baala.Overture.Type  as O


-- ============================================  Trim

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


-- ============================================  Padding

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


-- ============================================  Code point

-- | Test ASCII code point.
isAsciiCode :: (Integral n) => n -> Bool
isAsciiCode = (<= 127)

-- | Test newline code point.
isNewlineCode :: (Integral n) => n -> Bool
isNewlineCode n = (n == integralLF) || (n == integralCR)

-- | Generic code point (9) of horizontal tab character (@'\t'@).
integralHT :: (Integral n) => n
integralHT = 9

-- | Generic code point (10) of line feed character (@'\n'@).
integralLF :: (Integral n) => n
integralLF = 10

-- | Generic code point (11) of vertical tab character (@'\v'@).
integralVT :: (Integral n) => n
integralVT = 11

-- | Generic code point (12) of form feed character (@'\f'@).
integralFF :: (Integral n) => n
integralFF = 12

-- | Generic code point (13) of carriage return character (@'\r'@).
integralCR :: (Integral n) => n
integralCR = 13

-- | Generic code point (32) of space character (@' '@).
integralSpace :: (Integral n) => n
integralSpace = 32


-- ============================================  Put

-- | Print newline.
putLn :: IO ()
putLn = putStrLn ""

-- | Print newline.
hPutLn :: IO.Handle -> IO ()
hPutLn h = IO.hPutStrLn h ""

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

-- | Print prompt (@>>@) and read user input.
prompt :: IO String
prompt = do putStr ">> "
            IO.hFlush IO.stdout
            s <- getLine
            case trimBoth s of
              "" -> prompt
              s' -> return s'
