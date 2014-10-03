{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Msg



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
    [ C.CopCalc  (C.copInfix    "and")    copAnd
    , C.CopCalc  (C.copInfix    "or")     copOr
    , C.CopCalc  (C.copInfix    "then")   copImp
    , C.CopCalc  (C.copInfix    "when")   copWhen
    , C.CopCalc  (C.copNormal   "not")    copNot
    , C.CopCalc  (C.copNormal   "and")    copAnd
    , C.CopCalc  (C.copNormal   "or")     copOr
    , C.CopCalc  (C.copNormal   "then")   copImp
    , C.CopCalc  (C.copNormal   "when")   copWhen
    , C.CopCalc  (C.copInternal "#if")    copFunIf
    , C.CopTree  (C.copNormal   "if")     copTreeIf
    ]

cop1 :: (C.CBool c) => (Bool -> Bool) -> C.CopCalc c
cop1 p arg =
    do xc <- C.getArg1 arg
       x  <- C.getBool xc
       C.putBool $ p x

cop2 :: (C.CBool c) => (Bool -> Bool -> Bool) -> C.CopCalc c
cop2 p arg =
    do (xc, yc) <- C.getArg2 arg
       x <- C.getBool xc
       y <- C.getBool yc
       C.putBool $ p x y

copN :: (C.CBool c) => Bool -> (Bool -> Bool -> Bool) -> C.CopCalc c
copN unit p = loop where
    loop []   = C.putBool unit
    loop [xc] = xc
    loop (xc1 : xc2 : xs) =
        do x1 <- C.getBool xc1
           x2 <- C.getBool xc2
           loop $ C.putBool (p x1 x2) : xs

copNot :: (C.CBool c) => C.CopCalc c
copNot =  cop1 not

copWhen  :: (C.CBool c) => C.CopCalc c
copWhen  =  cop2 $ \x y -> x || not y

copImp :: (C.CBool c) => C.CopCalc c
copImp =  cop2 $ \x y -> not x || y

copAnd :: (C.CBool c) => C.CopCalc c
copAnd =  copN True (&&)

copOr  :: (C.CBool c) => C.CopCalc c
copOr  =  copN False (||)


-- ----------------------  if

nameLeaf :: B.BlankName -> B.TTree
nameLeaf = B.TreeL . B.TName B.codeZero

treeIf :: B.TTree -> B.TTree -> B.TTree -> B.TTree
treeIf test con alt = B.wrapTrees [ nameLeaf $ C.copInternal "#if" , test, con , alt ]

treeOrList :: [B.TTree] -> B.TTree
treeOrList [x] = x
treeOrList xs = B.wrapTrees $ (nameLeaf $ C.copNormal "or") : xs

copFunIf  :: (C.CBool c, C.CEmpty c) => C.CopCalc c
copFunIf arg =
    do (testC, conC, altC) <- C.getArg3 arg
       test <- C.getBool testC
       case test of
         True  -> conC
         False -> altC

--  if TEST -> CON : ALT 
--  if TEST -> CON
--  if TEST -> CON : TEST -> CON : TEST -> CON
--  if : TEST -> CON : TEST -> CON : TEST -> CON

copTreeIf :: C.CopTree
copTreeIf trees = folding $ filter (/= []) $ B.divideTreesBy ":" trees where
    folding :: [[B.TTree]] -> B.Ab B.TTree
    folding []        = Right $ B.TreeL $ B.textToken "()"
    folding (x : xs)  = fore x =<< folding xs

    fore :: [B.TTree] -> B.AbMap B.TTree
    fore trees2 alt =
        case B.divideTreesBy "->" trees2 of
          [_]         -> back trees2 alt
          [test, con] -> do test2 <- stairs ">>" "<<" test
                            Right $ treeIf test2 (B.wrapTrees con) alt
          _           -> abortSyntax trees2 "Expect E -> E"

    back :: [B.TTree] -> B.AbMap B.TTree
    back trees2 alt =
        case B.divideTreesBy "<-" trees2 of
          [alt2]      -> Right $ B.wrapTrees alt2
          [con, test] -> do test2 <- stairs "<<" ">>" test
                            Right $ treeIf test2 (B.wrapTrees con) alt
          _           -> abortSyntax trees2 "Expect E <- E"

    stairs :: String -> String -> [B.TTree] -> B.Ab B.TTree
    stairs del del2 xs =
        do notInclude del2 xs
           Right $ treeOrList $ map B.wrapTrees $ B.divideTreesBy del xs

    notInclude :: String -> [B.TTree] -> B.Ab ()
    notInclude del xs =
        case B.divideTreesBy del xs of
          [_] -> Right ()
          _   -> abortSyntax xs "Mixed arrows"

    abortSyntax xs msg =
        B.abortableTrees "if" xs $
         Msg.unexpAttr msg

