{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copArith
-- $Operators
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Core.Content

import Koshucode.Baala.Vanilla.Value.Content



-- ----------------------

litInt :: [TokenTree] -> AbOr VContent
litInt [TreeL (TWord _ _ n)] = readIntVal n
litInt xs = Left $ AbortNotNumber (show xs)

readIntVal :: String -> AbOr VContent
readIntVal s =
    case reads s of
      [(n, "")] -> Right $ VInt n
      _         -> Left $ AbortNotNumber s

readInt :: String -> AbOr Int
readInt s =
    case reads s of
      [(n, "")] -> Right $ n
      _         -> Left $ AbortNotNumber s




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
 , namedLit    "int"  litInt
 ]

arithInt :: VContent -> AbOr Int
arithInt (VInt    n) = Right n
arithInt (VString n) = Right =<< readInt n
arithInt x = Left $ AbortNotNumber (show x)

arithPlus :: [VContent] -> AbOr VContent
arithPlus xs = fmap VInt $ loop xs where
    loop [] = Right 0
    loop (n : m) = do n' <- arithInt n
                      m' <- loop m
                      Right $ n' + m'

arithTimes :: [VContent] -> AbOr VContent
arithTimes xs = fmap VInt $ loop xs where
    loop [] = Right 1
    loop (n : m) = do n' <- arithInt n
                      m' <- loop m
                      Right $ n' * m'

arithMinus :: [VContent] -> AbOr VContent
arithMinus [a]    = do a' <- arithInt a
                       Right . VInt $ - a'
arithMinus [a, b] = do a' <- arithInt a
                       b' <- arithInt b
                       Right . VInt $ a' - b'
arithMinus _ = Left $ AbortMalformedOperand "-"

arithAbs :: [VContent] -> AbOr VContent
arithAbs [VList cs] = Right . VList =<< mapM arithAbs1 cs
arithAbs [c] = arithAbs1 c
arithAbs _ = Left AbortUnmatchArity

arithAbs1 :: VContent -> AbOr VContent
arithAbs1 (VInt n) = Right . VInt $ abs n
arithAbs1 _ = Left AbortUnmatchArity

-- let tree = singleTree . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runCox (e2 $ Relhead []) []
-- let Left  e3 = runCox e2 []

