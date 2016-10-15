{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Utility
  ( -- * Trim
    trimLeft, trimRight, trimBoth,
    -- * Padding
    padRight, padRightWith,
    padLeft, padLeftWith,
    stringWidth,
    -- * Put
    putShow, putShowLn,
    putLines, hPutLines, hPutEmptyLine,
    -- * Read
    readInt, readInteger,
  ) where

import qualified Data.Char                     as Ch
import qualified System.IO                     as IO
import qualified Koshucode.Baala.Overture      as O


-- ----------------------  Trim

isSpace :: Char -> Bool
isSpace c = Ch.isSpace c    -- UnicodeSeprator | UnicodeOther

trimLeft :: O.Map String
trimLeft = dropWhile isSpace

trimRight :: O.Map String
trimRight [] = []
trimRight (x : xs) =
    case x : trimRight xs of
      [y] | isSpace y -> []
      ys -> ys

trimBoth :: O.Map String
trimBoth = trimRight . trimLeft


-- ----------------------  Padding

-- | Add spaces to right.
--
--   >>> padRight 10 "abc"
--   "abc       "
padRight :: Int -> O.Map String
padRight = padRightWith ' '

padRightWith :: Char -> Int -> O.Map String
padRightWith p n s = s ++ replicate rest p where
    rest = max 0 (n - stringWidth s)

-- | Add spaces to left.
--
--   >>> padLeft 10 "abc"
--   "       abc"
padLeft :: Int -> O.Map String
padLeft = padLeftWith ' '

padLeftWith :: Char -> Int -> O.Map String
padLeftWith p n s = replicate rest p ++ s where
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

hPutLines :: IO.Handle -> [String] -> IO ()
hPutLines h = (IO.hPutStrLn h `mapM_`)

hPutEmptyLine :: IO.Handle -> IO ()
hPutEmptyLine h = IO.hPutStrLn h ""


-- ----------------------  Read

readJust :: (Read a) => String -> Maybe a
readJust s = case reads s of
              [(x, "")] -> Just x
              _         -> Nothing

readInt :: String -> Maybe Int
readInt = readJust

readInteger :: String -> Maybe Integer
readInteger = readJust

