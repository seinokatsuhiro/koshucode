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

import Koshucode.Baala.Base
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
    do eg egLit
       eg egArith
       
       

egLit :: IO ()
egLit =
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

egArith :: IO ()
egArith =
  do "add"      >>> bin decimalAdd "5     7"
     "add"      >>> bin decimalAdd "5.0   7.0"
     "add"      >>> bin decimalAdd "8.5   7.5"
     "add"      >>> bin decimalAdd "8.5   7.8"

                    -- 5 + 7 = 12             5:0 + 7:0 = 12:0
                    -- 5.0 + 7.0 = 12.0       50:1 + 70:1 = 120:1

     "sub"      >>> bin decimalSub "10     3"
     "sub"      >>> bin decimalSub "10.0   3.0"
     "sub"      >>> bin decimalSub "10.0   0.5"
     "sub"      >>> bin decimalSub " 5.0  10.5"

     "mul"      >>> bin decimalMul "10     3"
     "mul"      >>> bin decimalMul "10.0   3.0"
     "mul"      >>> bin decimalMul "10.0   0.5"
     "mul"      >>> bin decimalMul " 5.0  10.5"
     "mul"      >>> bin decimalMul "10    10.5"

                    -- 10 * 3 = 30            10:0 * 3:0 = 30:0
                    -- 10.0 * 3.0 = 30.0      100:1 * 30:1 = 3000:2
                    -- 10.00 * 3.00 = 30.00   1000:2 * 300:2 = 300000:4
                    -- 10.00 * 3 = 30.00      1000:2 * 3:0 = 3000:2

     "div"      >>> bin decimalDiv "10     3"
     "div"      >>> bin decimalDiv "10.0   3.0"
     "div"      >>> bin decimalDiv "10.0   0.5"
     "div"      >>> bin decimalDiv " 5.0  10.5"

                    -- 10 / 3 = 3             10:0 / 3:0 = 3:0
                    -- 10.0 / 3.0 = 30.0      100:1 / 30:1 = 3:0
                    -- 10.00 / 3.00 = 30.00   1000:2 / 300:2 = 3:0

bin :: (Decimal -> AbMap Decimal) -> AbMap String
bin f xy =
    do let [x,y] = words xy
       dx <- litDecimal x
       dy <- litDecimal y
       dz <- f dx dy
       Right $ decimalString dz
       --Right $ show dz

