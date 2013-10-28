#!/usr/bin/env koshu-env.sh runhaskell
--
--  DESCRIPTION
--    Examples of hash words
--
--  USAGE
--    chmod 755 eg-hash.hs
--    ./eg-hash.hs > eg-hash.log
--

{-# OPTIONS_GHC -Wall #-}

import qualified Koshucode.Baala.Core as C



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
    do "no hash char"   >>> C.hashWord "abc"
       "single quote"   >>> C.hashWord "abc's"
       "single quote"   >>> C.hashWord "'abc'"
       "doule quote"    >>> C.hashWord "\"abc\""
       "doule quote"    >>> C.hashWord "''abc''"
       "empty string"   >>> C.hashWord ""
       "newline"        >>> C.hashWord "abc\r\ndef"
       "newline"        >>> C.hashWord "abc def"

-- hash :: String -> String
-- hash = hashString
