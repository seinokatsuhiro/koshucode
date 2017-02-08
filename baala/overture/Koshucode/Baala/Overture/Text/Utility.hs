{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text utility.

module Koshucode.Baala.Overture.Text.Utility
  ( -- * Reexport from Data.Char
    Ch.isSpace,

    -- * Trim
    trimBegin, trimEnd, trimBoth,
    sweep, sweepAll,

    -- * Padding
    padBegin, padEnd, 
    padBeginWith, padEndWith,
    stringWidth,
    addSpace,

    -- * Code point
    isAsciiCode,
    isControlCode,
    isFormatCode,
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
    prompt, promptWith,
  ) where

import qualified Data.Char                              as Ch
import qualified System.IO                              as IO
import qualified Koshucode.Baala.Overture.Infix         as O
import qualified Koshucode.Baala.Overture.Type          as O
import qualified Koshucode.Baala.Overture.Text.Textual  as O


-- ============================================  Trim

-- | Remove space and space-like characters from the beginning of string.
--
--   >>> trimBegin "  abc  "
--   "abc  "
--
trimBegin :: (O.Textual t) => t -> t
trimBegin = tDropWhile Ch.isSpace

tDropWhile :: (O.Textual t) => O.Test Char -> t -> t
tDropWhile f = loop where
    loop (O.tCut -> Just (c, t)) | f c  = loop t
    loop t = t

-- | Remove space and space-like characters from the end of string.
--
--   >>> trimEnd "  abc  "
--   "  abc"
--
trimEnd :: (O.Textual t) => t -> t
trimEnd (O.tCut -> Just (x, xs)) =
    let ys = x O.<:> trimEnd xs
    in case O.tCut2 ys of
         Just (y, Nothing) | Ch.isSpace y -> O.tEmpty
         _ -> ys
trimEnd t = t

-- | Remove space and space-like characters
--   from the beginning and end of string.
--
--   >>> trimBoth "  abc  "
--   "abc"
--
trimBoth :: (O.Textual t) => t -> t
trimBoth = trimEnd . trimBegin

-- | Trim and shorten consecutive spaces.
--
--   >>> sweep "  foo  bar   baz "
--   "foo bar baz"
--
sweep :: (O.Textual t) => t -> t
sweep = loop . trimBegin where
    loop (O.tCut -> Just(c, t))
        | Ch.isSpace c  = case loop $ trimBegin t of
                            t' | O.tIsEmpty t' -> O.tEmpty
                               | otherwise     -> ' ' O.<:> t'
        | otherwise     = c O.<:> loop t
    loop t = t

-- | Delete space characters.
--
--   >>> sweepAll "  foo  bar   baz "
--   "foobarbaz"
--
sweepAll :: (O.Textual t) => t -> t
sweepAll (O.tCut -> Just (c, t))
    | Ch.isSpace c  = sweepAll t
    | otherwise     = c O.<:> sweepAll t
sweepAll t = t


-- ============================================  Padding

-- | Add spaces to the left.
--
--   >>> padBegin 10 "abc"
--   "       abc"
--
padBegin :: (O.Textual t) => Int -> t -> t
padBegin = padBeginWith ' '

-- | Add spaces to the right.
--
--   >>> padEnd 10 "abc"
--   "abc       "
--
padEnd :: (O.Textual t) => Int -> t -> t
padEnd = padEndWith ' '

-- | Add given character to the left.
--
--   >>> padBeginWith '.' 10 "abc"
--   ".......abc"
--
padBeginWith :: (O.Textual t) => Char -> Int -> t -> t
padBeginWith p n t = O.charsT rest p O.++ t where
    rest = max 0 (n - stringWidth t)

-- | Add given character to the right.
padEndWith :: (O.Textual t) => Char -> Int -> t -> t
padEndWith p n t = t O.++ O.charsT rest p where
    rest = max 0 (n - stringWidth t)

-- | Calculate width of string.
stringWidth :: (O.Textual t) => t -> Int
stringWidth = sum . O.tList charWidth

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
addSpace :: (O.Textual t) => t -> t
addSpace cs@(O.tCut -> Just (c, _))
    | Ch.isSpace c   = cs
    | otherwise      = ' ' O.<:> cs
addSpace _           = O.tEmpty


-- ============================================  Code point

-- | Test ASCII code point (0 .. 127).
isAsciiCode :: (Integral n) => n -> Bool
isAsciiCode = (<= 127)

-- | Test ASCII control code point (0 .. 31, 127).
isControlCode :: (Integral n) => n -> Bool
isControlCode n = n <= 31 || n == 127

-- | Test format code point (9 .. 13).
isFormatCode :: (Integral n) => n -> Bool
isFormatCode n = n >= integralHT && n <= integralCR

-- | Test newline code point (10, 13).
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
--   It returns trimmed input string.
--   Reread when input is empty.
prompt :: IO String
prompt = promptWith ">> "

-- | Print prompt with give string and read user input.
promptWith :: String -> IO String
promptWith p =
    do putStr p
       IO.hFlush IO.stdout
       s <- getLine
       case trimBoth s of
         "" -> promptWith p
         s' -> return s'
