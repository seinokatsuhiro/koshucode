{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Message



-- ----------------------
-- $Operators
--
--  [@not@]    Logical negation.
--
--  [@and@]    Logical conjunction.
--
--  [@or@]     Logical disjunction.
--
--  [@then@]   Logical implication.
--
--  [@when@]   Inverse implication.
--
--  [@if@]     Conditional expression.
--

copsLogic :: (C.CBool c, C.CEmpty c) => [C.Cop c]
copsLogic =
    [ C.CopFun   (C.copInfix  "and")    copAnd
    , C.CopFun   (C.copInfix  "or")     copOr
    , C.CopFun   (C.copInfix  "then")   copImp
    , C.CopFun   (C.copInfix  "when")   copWhen
    , C.CopFun   (C.copNormal "not")    copNot
    , C.CopFun   (C.copNormal "and")    copAnd
    , C.CopFun   (C.copNormal "or")     copOr
    , C.CopFun   (C.copNormal "then")   copImp
    , C.CopFun   (C.copNormal "when")   copWhen
    , C.CopFun   (C.copNormal "#if")    copIf
    , C.CopTree  (C.copNormal "if")     synIf
    ]

cop1 :: (C.CBool c) => (Bool -> Bool) -> C.CopFun c
cop1 p arg =
    do xc <- C.getArg1 arg
       x  <- C.getBool xc
       C.putBool $ p x

cop2 :: (C.CBool c) => (Bool -> Bool -> Bool) -> C.CopFun c
cop2 p arg =
    do (xc, yc) <- C.getArg2 arg
       x <- C.getBool xc
       y <- C.getBool yc
       C.putBool $ p x y

copN :: (C.CBool c) => Bool -> (Bool -> Bool -> Bool) -> C.CopFun c
copN unit p = loop where
    loop []   = C.putBool unit
    loop [xc] = xc
    loop (xc1 : xc2 : xs) =
        do x1 <- C.getBool xc1
           x2 <- C.getBool xc2
           loop $ C.putBool (p x1 x2) : xs

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

-- ----------------------  if

treeOp :: String -> B.TokenTree
treeOp = B.TreeL . B.textToken

treeIf :: B.TokenTree -> B.TokenTree -> B.TokenTree -> B.TokenTree
treeIf test con alt = B.treeWrap [ treeOp "#if" , test, con , alt ]

treeOrList :: [B.TokenTree] -> B.TokenTree
treeOrList [x] = x
treeOrList xs = B.treeWrap $ (treeOp "or") : xs

copIf  :: (C.CBool c, C.CEmpty c) => C.CopFun c
copIf arg =
    do (testC, conC, altC) <- C.getArg3 arg
       test <- C.getBool testC
       case test of
         True  -> conC
         False -> altC

--  if TEST -> CON : ALT 
--  if TEST -> CON
--  if TEST -> CON : TEST -> CON : TEST -> CON
--  if : TEST -> CON : TEST -> CON : TEST -> CON

synIf :: C.CopTree
synIf trees = folding $ filter (/= []) $ B.divideTreesBy ":" trees where
    folding :: [[B.TokenTree]] -> B.Ab B.TokenTree
    folding []        = Right $ B.TreeL $ B.textToken "()"
    folding (x : xs)  = fore x =<< folding xs

    fore :: [B.TokenTree] -> B.AbMap B.TokenTree
    fore trees2 alt =
        case B.divideTreesBy "->" trees2 of
          [_]         -> back trees2 alt
          [test, con] -> do test2 <- stairs ">>" "<<" test
                            Right $ treeIf test2 (B.treeWrap con) alt
          _           -> abortSyntax trees2 "Expect E -> E"

    back :: [B.TokenTree] -> B.AbMap B.TokenTree
    back trees2 alt =
        case B.divideTreesBy "<-" trees2 of
          [alt2]      -> Right $ B.treeWrap alt2
          [con, test] -> do test2 <- stairs "<<" ">>" test
                            Right $ treeIf test2 (B.treeWrap con) alt
          _           -> abortSyntax trees2 "Expect E <- E"

    stairs :: String -> String -> [B.TokenTree] -> B.Ab B.TokenTree
    stairs del del2 xs =
        do notInclude del2 xs
           Right $ treeOrList $ map B.treeWrap $ B.divideTreesBy del xs

    notInclude :: String -> [B.TokenTree] -> B.Ab ()
    notInclude del xs =
        case B.divideTreesBy del xs of
          [_] -> Right ()
          _   -> abortSyntax xs "Mixed arrows"

    abortSyntax xs msg =
        B.abortableTrees "if" xs $
         Message.unexpAttr msg

