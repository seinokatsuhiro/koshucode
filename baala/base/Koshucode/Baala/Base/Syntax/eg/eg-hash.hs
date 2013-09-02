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
    do "no hash char"   >>> hashBack "abc"
       "single quote"   >>> hashBack "abc's"
       "single quote"   >>> hashBack "'abc'"
       "doule quote"    >>> hashBack "\"abc\""
       "doule quote"    >>> hashBack "''abc''"
       "empty string"   >>> hashBack ""
       "newline"        >>> hashBack "abc\r\ndef"
       "newline"        >>> hashBack "abc def"

-- hash :: String -> String
-- hash = hashString
