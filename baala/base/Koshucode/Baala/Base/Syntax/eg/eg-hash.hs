#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of hash words
--
--  USAGE
--    chmod 755 eg-hash.hs
--    ./eg-hash.hs > eg-hash.log
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Base



-- ----------------------  Utility

(>>>) :: (Show a) => String -> a -> IO ()
(>>>) n x = let len  = length n
                dots = concat $ repeat " . "
                fill = take (20 - len) dots
            in  putStrLn $ n ++ " " ++ fill ++ " " ++ show x

eg :: IO () -> IO ()
eg f = do f
          putStrLn ""



-- ----------------------  Main

main :: IO ()
main =
    do eg egHash

egHash :: IO ()
egHash =
    do "no hash char"   >>> hash "abc"
       "single quote"   >>> hash "abc's"
       "single quote"   >>> hash "'abc'"
       "doule quote"    >>> hash "\"abc\""
       "doule quote"    >>> hash "''abc''"
       "empty string"   >>> hash ""
       "newline"        >>> hash "abc\r\ndef"
       "newline"        >>> hash "abc def"

hash :: String -> [String]
hash = hashSplit

-- hash :: String -> String
-- hash = hashString
