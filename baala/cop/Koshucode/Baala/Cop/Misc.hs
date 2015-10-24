{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Misc
  ( copsMisc
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Cop.Coxhand     as H
import qualified Koshucode.Baala.Cop.Message     as Msg


-- ----------------------
-- $Operators
--
--  [@if@]        Conditional expression.
--
--  [@is@]        @x is f@ == @f x@.
--
--  [@of@]        @f of x@ == @f x@.
--
--  [@to@]        @x to f@ == @f x@.
--
--  [@type@]      Type of content.
--

copsMisc :: (D.CBool c, D.CEmpty c, D.CType c) => [D.Cop c]
copsMisc =
    [ D.CopCalc  (D.copNormal   "type")    copType

    , D.CopCox   (D.copInfix    "is")      toInfix
    , D.CopCox   (D.copInfix    "of")      ofInfix
    , D.CopCox   (D.copInfix    "to")      toInfix

    , D.CopCalc  (D.copInternal "#if")     copFunIf
    , D.CopTree  (D.copNormal   "if")      copTreeIf
    ]


-- ----------------------  type

copType :: (D.CType c, D.CTypeOf c) => D.CopCalc c
copType arg =
    do x <- D.getRightArg1 arg
       D.putType $ D.typeOf x

-- ----------------------  is, of, to

ofInfix :: D.CopCox c
ofInfix [f, x] = Right $ H.ix f [x]
ofInfix _ = Msg.adlib "require operand"

toInfix :: D.CopCox c
toInfix [x, f] = Right $ H.ix f [x]
toInfix _ = Msg.adlib "require operand"

-- ----------------------  if

nameLeaf :: D.BlankName -> D.TTree
nameLeaf = B.TreeL . D.TName B.codePtZero

treeIf :: D.TTree -> D.TTree -> D.TTree -> D.TTree
treeIf test con alt = D.ttreeGroup [ nameLeaf $ D.copInternal "#if" , test, con , alt ]

treeOrList :: [D.TTree] -> D.TTree
treeOrList [x] = x
treeOrList xs = D.ttreeGroup $ (nameLeaf $ D.copNormal "or") : xs

copFunIf  :: (D.CBool c, D.CEmpty c) => D.CopCalc c
copFunIf arg =
    do (testC, conC, altC) <- D.getArg3 arg
       test <- testC
       testB <- D.getBool $ Right test
       case testB of
         True  -> conC
         False -> altC

--  if TEST -> CON : ALT 
--  if TEST -> CON
--  if TEST -> CON : TEST -> CON : TEST -> CON
--  if : TEST -> CON : TEST -> CON : TEST -> CON

copTreeIf :: D.CopTree
copTreeIf trees = folding $ filter (/= []) $ D.divideTreesBy ":" trees where
    folding :: [[D.TTree]] -> B.Ab D.TTree
    folding []        = Right $ B.TreeL $ D.textToken "()"
    folding (x : xs)  = fore x =<< folding xs

    fore :: [D.TTree] -> B.AbMap D.TTree
    fore trees2 alt =
        case D.divideTreesBy "->" trees2 of
          [_]         -> back trees2 alt
          [test, con] -> do test2 <- stairs ">>" "<<" test
                            Right $ treeIf test2 (D.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E -> E"

    back :: [D.TTree] -> B.AbMap D.TTree
    back trees2 alt =
        case D.divideTreesBy "<-" trees2 of
          [alt2]      -> Right $ D.ttreeGroup alt2
          [con, test] -> do test2 <- stairs "<<" ">>" test
                            Right $ treeIf test2 (D.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E <- E"

    stairs :: String -> String -> [D.TTree] -> B.Ab D.TTree
    stairs del del2 xs =
        do notInclude del2 xs
           Right $ treeOrList $ map D.ttreeGroup $ D.divideTreesBy del xs

    notInclude :: String -> [D.TTree] -> B.Ab ()
    notInclude del xs =
        case D.divideTreesBy del xs of
          [_] -> Right ()
          _   -> abortSyntax xs "Mixed arrows"

    abortSyntax xs msg =
        Msg.abortableTrees "if" xs $
         Msg.unexpAttr msg

