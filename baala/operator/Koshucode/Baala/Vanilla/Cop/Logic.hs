{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------
{- $Operators

 [@not@]    Logical negation.

 [@and@]    Logical conjunction.

 [@or@]     Logical disjunction.

 [@then@]   Logical implication.

 [@when@]   Inverse implication.

 [@if@]     Conditional expression.

-}

copsLogic :: (C.CBool c, C.CNil c) => [C.Cop c]
copsLogic =
    [ C.CopFun  "not"   copNot
    , C.CopFun  "and"   copAnd
    , C.CopFun  "or"    copOr
    , C.CopFun  "then"  copImp
    , C.CopFun  "when"  copWhen
    , C.CopFun  "/if"   copIf
    , C.CopSyn  "if"    synIf
    ]

typeUnmatch :: (C.PrimContent a) => [a] -> B.Ab b
typeUnmatch xs = Left $ B.AbortCalc [] $ B.ACUnmatchType (concatMap C.typename xs)

cop1 :: (C.CBool c) => (Bool -> Bool) -> C.CopFun c
cop1 p [x] =
    do x' <- C.needBool x
       Right . C.putBool $ p x'
cop1 _ xs = typeUnmatch xs

cop2 :: (C.CBool c) => (Bool -> Bool -> Bool) -> C.CopFun c
cop2 p [x, y] =
    do x' <- C.needBool x
       y' <- C.needBool y
       Right . C.putBool $ p x' y'
cop2 _ xs = typeUnmatch xs

copN :: (C.CBool c) => Bool -> (Bool -> Bool -> Bool) -> C.CopFun c
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (x : xs) =
        do x' <- C.needBool x
           y  <- loop xs 
           y' <- C.needBool y
           Right . C.putBool $ p x' y'

copNot :: (C.CBool c) => C.CopFun c
copNot =  cop1 not

copWhen  :: (C.CBool c) => C.CopFun c
copWhen  =  cop2 $ \x y -> x || not y

copImp :: (C.CBool c) => C.CopFun c
copImp =  cop2 $ \x y -> not x || y

copAnd :: (C.CBool c) => C.CopFun c
copAnd =  copN True (&&)

copOr  :: (C.CBool c) => C.CopFun c
copOr  =  copN False (||)

copIf  :: (C.CBool c, C.CNil c) => C.CopFun c
copIf [test, a, b] =
    do test' <- C.needBool test
       case test' of
         True  -> Right a
         False -> Right b
copIf [test, a] = copIf [test, a, C.nil]
copIf xs = typeUnmatch xs

funIf :: B.TokenTree
funIf = B.TreeL $ B.tokenWord "/if"

--  if TEST -> CON : ALT 
--  if TEST -> CON
--  if TEST -> CON : TEST -> CON : TEST -> CON
--  if : TEST -> CON : TEST -> CON : TEST -> CON

synIf :: C.CopSyn
synIf trees = folding $ B.divideTreesBy ":" trees where
    folding :: [[B.TokenTree]] -> B.Ab B.TokenTree
    folding []        = Right $ B.TreeL $ B.tokenWord "()"
    folding ([] : xs) = folding xs
    folding (x : xs)  = fore x =<< folding xs

    fore :: [B.TokenTree] -> B.AbMap B.TokenTree
    fore trees2 alt =
        case B.divideTreesBy "->" trees2 of
          [_]         -> back trees2 alt
          [test, con] -> Right $ B.treeWrap [funIf, (B.treeWrap test), B.treeWrap con, alt]
          _           -> Left  $ B.abortOperand "if"

    back :: [B.TokenTree] -> B.AbMap B.TokenTree
    back trees2 alt =
        case B.divideTreesBy "<-" trees2 of
          [alt2]      -> Right $ B.treeWrap alt2
          [con, test] -> Right $ B.treeWrap [funIf, (B.treeWrap test), B.treeWrap con, alt]
          _           -> Left  $ B.abortOperand "if"

