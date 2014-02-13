{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V



-- ----------------------
{- $Operators

 [@not@]    Logical negation.

 [@and@]    Logical conjunction.

 [@or@]     Logical disjunction.

 [@then@]   Logical implication.

 [@when@]   Inverse implication.

-}

copsLogic :: [C.Cop V.VContent]
copsLogic =
    [ C.CopFun  "not"   copNot
    , C.CopFun  "and"   copAnd
    , C.CopFun  "or"    copOr
    , C.CopFun  "then"  copImp
    , C.CopFun  "when"  copWhen
    , C.CopFun  "/if"   copIf
    , C.CopSyn  "if"    synIf
    , C.CopSyn  "case"  synCase
    ]

cop1 :: (Bool -> Bool) -> V.VCop
cop1 p [x] =
    do x' <- C.needBool x
       Right . C.putBool $ p x'
cop1 _ xs = typeUnmatch xs

cop2 :: (Bool -> Bool -> Bool) -> V.VCop
cop2 p [x, y] =
    do x' <- C.needBool x
       y' <- C.needBool y
       Right . C.putBool $ p x' y'
cop2 _ xs = typeUnmatch xs

copN :: Bool -> (Bool -> Bool -> Bool) -> V.VCop
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (x : xs) =
        do x' <- C.needBool x
           V.VBool xs' <- loop xs 
           Right . C.putBool $ p x' xs'

copNot :: V.VCop
copNot =  cop1 not

copWhen  :: V.VCop
copWhen  =  cop2 $ \x y -> x || not y

copImp :: V.VCop
copImp =  cop2 $ \x y -> not x || y

copAnd :: V.VCop
copAnd =  copN True (&&)

copOr  :: V.VCop
copOr  =  copN False (||)

copIf  :: V.VCop
copIf [test, a, b] =
    do test' <- C.needBool test
       case test' of
         True  -> Right a
         False -> Right b
copIf [test, a] = copIf [test, a, C.nil]
copIf xs = typeUnmatch xs

funIf :: B.TokenTree
funIf = B.TreeL $ B.tokenWord "/if"

typeUnmatch :: C.PrimContent a => [a] -> Either B.AbortReason b
typeUnmatch xs = Left $ B.AbortCalc [] $ B.ACUnmatchType (concatMap C.typename xs)

--  if TEST -> CON        =  /if TEST CON
--  if TEST -> CON : ALT  =  /if TEST CON ALT

synIf :: C.CopSyn
synIf trees =
    case B.divideTreesBy "->" trees of
      [test, result] -> synIfResult (B.treeWrap test) result
      _ -> Left $ B.abortOperand "if"

synIfResult :: B.TokenTree -> C.CopSyn
synIfResult test result =
    case B.divideTreesBy ":" result of
      [con, alt] -> Right $ B.treeWrap [funIf, test, B.treeWrap con, B.treeWrap alt]
      [con]      -> Right $ B.treeWrap [funIf, test, B.treeWrap con]
      _          -> Left $ B.abortOperand "if"

--  case TEST -> CON : TEST -> CON  =  /if TEST CON (/if TEST CON)

synCase :: C.CopSyn
synCase trees = folding $ B.divideTreesBy ":" trees where
    folding :: [[B.TokenTree]] -> B.Ab B.TokenTree
    folding []        = Right $ B.TreeL $ B.tokenWord "()"
    folding ([] : xs) = folding xs
    folding (x : xs)  = cond x =<< folding xs

    cond :: [B.TokenTree] -> B.TokenTree -> B.Ab B.TokenTree
    cond trees2 alt =
        case B.divideTreesBy "->" trees2 of
          [test, con] -> Right $ B.treeWrap [funIf, (B.treeWrap test), B.treeWrap con, alt]
          _  -> Left $ B.abortOperand "case"

