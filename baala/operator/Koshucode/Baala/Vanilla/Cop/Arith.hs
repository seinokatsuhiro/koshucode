{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copArith
-- $Operators
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Vanilla.Type.Content



-- ----------------------

-- readInt :: String -> AbOr Int
-- readInt s =
--     case reads s of
--       [(n, "")] -> Right $ n
--       _         -> Left $ AbortNotNumber s




-- ----------------------
{- $Operators

 [@+@]     Addition.

 [@-@]     Subtruction.

 [@*@]     Multipication.

 [@abs@]   Absolute value.

-}

copArith :: [Named (Cop VContent)]
copArith =
    [ namedEager  "+"    arithPlus
    , namedEager  "*"    arithTimes
    , namedEager  "-"    arithMinus
    , namedEager  "abs"  arithAbs
    ]

arithDec :: VContent -> AbOr Decimal
arithDec (VDec  n) = Right n
arithDec (VText n) = litDecimal n
arithDec x = Left $ AbortNotNumber (show x)

arithPlus :: [VContent] -> AbOr VContent
arithPlus xs = fmap VDec $ loop xs where
    loop [] = Right $ intDecimal 0
    loop (n : m) = do n' <- arithDec n
                      m' <- loop m
                      decimalAdd n' m'

arithTimes :: [VContent] -> AbOr VContent
arithTimes xs = fmap VDec $ loop xs where
    loop [] = Right $ intDecimal 1
    loop (n : m) = do n' <- arithDec n
                      m' <- loop m
                      decimalMul n' m'

arithMinus :: [VContent] -> AbOr VContent
arithMinus [a] =
    do a' <- arithDec a
       Right . VDec $ decimalRevsign a'
arithMinus [a, b] =
    do a' <- arithDec a
       b' <- arithDec b
       c' <- decimalSub a' b'
       Right . VDec $ c'
arithMinus _ = Left $ AbortMalformedOperand "-"

arithAbs :: [VContent] -> AbOr VContent
arithAbs [VList cs] = Right . VList =<< mapM arithAbs1 cs
arithAbs [c] = arithAbs1 c
arithAbs _ = Left AbortUnmatchArity

arithAbs1 :: VContent -> AbOr VContent
arithAbs1 (VDec n) = Right . VDec $ decimalAbs n
arithAbs1 _ = Left AbortUnmatchArity

-- let tree = singleTree . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runCox (e2 $ Relhead []) []
-- let Left  e3 = runCox e2 []

