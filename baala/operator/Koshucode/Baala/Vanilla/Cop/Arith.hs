{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as Rop



-- ----------------------
{- $Operators

 [@+@]     Addition.

 [@-@]     Subtruction.

 [@*@]     Multipication.

 [@abs@]   Absolute value.

-}

copsArith :: [B.Named (C.Cop Rop.VContent)]
copsArith =
    [ C.namedEager  "+"    copPlus
    , C.namedEager  "*"    copTimes
    , C.namedEager  "-"    copMinus
    , C.namedEager  "quo"  copQuo
    , C.namedEager  "rem"  copRem
    , C.namedEager  "abs"  copAbs
    ]

copDec :: Rop.VContent -> B.Ab B.Decimal
copDec (Rop.VDec  n) = Right n
copDec (Rop.VText n) = B.litDecimal n
copDec x = Left $ B.AbortSyntax [] $ B.ASNotNumber (show x)

copPlus :: Rop.VCop
copPlus xs = fmap Rop.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: Rop.VCop
copTimes xs = fmap Rop.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: Rop.VCop
copMinus [a] =
    do a' <- copDec a
       Right . Rop.VDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right . Rop.VDec $ c'
copMinus _ = Left $ B.abortMalformedOperand "-"

copQuo :: Rop.VCop
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right . Rop.VDec $ c'
copQuo _ = Left $ B.abortMalformedOperand "quo"

copRem :: Rop.VCop
copRem [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalRem a' b'
       Right . Rop.VDec $ c'
copRem _ = Left $ B.abortMalformedOperand "rem"

copAbs :: Rop.VCop
copAbs [Rop.VList cs] = Right . Rop.VList =<< mapM copAbs1 cs
copAbs [c] = copAbs1 c
copAbs _ = Left $ B.abortMalformedOperand "abs"

copAbs1 :: Rop.VContent -> B.Ab Rop.VContent
copAbs1 (Rop.VDec n) = Right . Rop.VDec $ B.decimalAbs n
copAbs1 _ = Left $ B.abortMalformedOperand "abc"

-- let tree = treeG . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runCox (e2 $ Relhead []) []
-- let Left  e3 = runCox e2 []

