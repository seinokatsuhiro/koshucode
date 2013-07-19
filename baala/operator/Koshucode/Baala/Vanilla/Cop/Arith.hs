{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copArith
-- $Operators
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Value.Val



-- ----------------------

litInt :: [TokenTree] -> AbOr Val
litInt [TreeL (TWord _ _ n)] = readIntVal n
litInt xs = Left $ AbortNotNumber (show xs)

readIntVal :: String -> AbOr Val
readIntVal s =
    case reads s of
      [(n, "")] -> Right $ Intv n
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

copArith :: [Named (ContentOp Val)]
copArith =
 [ namedEager  "+"    arithPlus
 , namedEager  "*"    arithTimes
 , namedEager  "-"    arithMinus
 , namedEager  "abs"  arithAbs
 , namedLit    "int"  litInt
 ]

arithInt :: Val -> AbOr Int
arithInt (Intv    n) = Right n
arithInt (Stringv n) = Right =<< readInt n
arithInt x = Left $ AbortNotNumber (show x)

arithPlus :: [Val] -> AbOr Val
arithPlus xs = fmap Intv $ loop xs where
    loop [] = Right 0
    loop (n : m) = do n' <- arithInt n
                      m' <- loop m
                      Right $ n' + m'

arithTimes :: [Val] -> AbOr Val
arithTimes xs = fmap Intv $ loop xs where
    loop [] = Right 1
    loop (n : m) = do n' <- arithInt n
                      m' <- loop m
                      Right $ n' * m'

arithMinus :: [Val] -> AbOr Val
arithMinus [a]    = do a' <- arithInt a
                       Right . Intv $ - a'
arithMinus [a, b] = do a' <- arithInt a
                       b' <- arithInt b
                       Right . Intv $ a' - b'
arithMinus _ = Left $ AbortMalformedOperand "-"


arithAbs :: [Val] -> AbOr Val
arithAbs [Listv cs] = Right . Listv =<< mapM arithAbs1 cs
arithAbs [c] = arithAbs1 c
arithAbs _ = Left $ AbortLookup ""

arithAbs1 :: Val -> AbOr Val
arithAbs1 (Intv n) = Right . Intv $ abs n
arithAbs1 _ = Left $ AbortLookup ""

-- let tree = singleToken . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runContent (e2 $ Relhead []) []
-- let Left  e3 = runContent e2 []

