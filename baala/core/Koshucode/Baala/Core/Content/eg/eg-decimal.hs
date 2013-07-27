#!/usr/bin/env runhaskell
--
--  DESCRIPTION
--    Examples of decimal reader
--
--  USAGE
--    chmod 755 eg-decimal.hs
--    regress.sh eg-decimal.hs -l
--

{-# OPTIONS_GHC -Wall #-}

import Koshucode.Baala.Core



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
    do eg egDecimal

egDecimal :: IO ()
egDecimal =
  do "empty"      >>> litDecimal ""
     "digit"      >>> litDecimal "0"
     "digit"      >>> litDecimal "1"
     "sign"       >>> litDecimal "-"
     "sign"       >>> litDecimal "+"
     "decimal"    >>> litDecimal "12.34"
     "decimal"    >>> litDecimal "0.34"
     "decimal"    >>> litDecimal ".34"
     "plus"       >>> litDecimal "+ 12.34"
     "plus"       >>> litDecimal "12.34 +"
     "minus"      >>> litDecimal "- 12.34"
     "minus"      >>> litDecimal "12.34 -"
     "approx"     >>> litDecimal "12.34 a"
     "approx"     >>> litDecimal "12.34 - a"
     "space"      >>> litDecimal "12 800"
     "space"      >>> litDecimal "10 000 000"
     "max"        >>> litDecimal "1 000 000 000 000 000 000"
     "min"        >>> litDecimal "-1 000 000 000 000 000 000"

