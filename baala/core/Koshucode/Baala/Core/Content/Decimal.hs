{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Decimal
( LitString,
  litDecimal,  
) where

import Data.Char
import Koshucode.Baala.Base

{-| Make @a@ from a string. -}
type LitString a = AbMap2 String a

type LitDecimal = LitString (Int, Int, Bool)

litDecimal :: LitDecimal
litDecimal ccs = headPart id ccs where
    minus x = - x

    headPart _ [] = Right (0, 0, False)
    headPart sign (c:cs) = case c of
        ' '  ->  headPart sign  cs
        '-'  ->  headPart minus cs
        '+'  ->  headPart id    cs
        _    ->  intPart sign 0 (c:cs)

    intPart :: Map Int -> Int -> LitDecimal
    intPart sign n [] = Right (sign n, 0, False)
    intPart sign n (c:cs)
        | isDigit c  =  intPart sign (10 * n + fromDigit c) cs
        | c == ' '   =  intPart sign n cs
        | c == '.'   =  decPart sign n 0 cs
        | otherwise  =  tailPart False sign (n, 0) (c:cs)

    decPart :: Map Int -> Int -> Int -> LitDecimal
    decPart sign n p [] = Right (sign n, p, False)
    decPart sign n p (c:cs)
        | isDigit c  =  decPart sign (10 * n + fromDigit c) (p + 1) cs
        | c == ' '   =  decPart sign n p cs
        | otherwise  =  tailPart False sign (n, p) (c:cs)

    tailPart :: Bool -> Map Int -> (Int, Int) -> LitDecimal
    tailPart approx sign (n, p) [] = Right (sign n, p, approx)
    tailPart approx sign dec (c:cs) = case c of
        ' '  ->  tailPart approx sign  dec cs
        '-'  ->  tailPart approx minus dec cs
        '+'  ->  tailPart approx id    dec cs
        'a'  ->  tailPart True   sign  dec cs
        'A'  ->  tailPart True   sign  dec cs
        _    ->  Left $ AbortNotNumber ccs

fromDigit :: Char -> Int
fromDigit '0' = 0
fromDigit '1' = 1
fromDigit '2' = 2
fromDigit '3' = 3
fromDigit '4' = 4
fromDigit '5' = 5
fromDigit '6' = 6
fromDigit '7' = 7
fromDigit '8' = 8
fromDigit '9' = 9
fromDigit _   = bug

