{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Misc
  ( copsMisc
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Cop.Coxhand  as H
import qualified Koshucode.Baala.Op.Message      as Msg


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

copsMisc :: (C.CBool c, C.CEmpty c, C.CType c) => [C.Cop c]
copsMisc =
    [ C.CopCalc  (C.copNormal   "type")    copType

    , C.CopCox   (C.copInfix    "is")      toInfix
    , C.CopCox   (C.copInfix    "of")      ofInfix
    , C.CopCox   (C.copInfix    "to")      toInfix

    , C.CopCalc  (C.copInternal "#if")     copFunIf
    , C.CopTree  (C.copNormal   "if")      copTreeIf
    ]


-- ----------------------  type

copType :: (C.CType c, C.CTypeOf c) => C.CopCalc c
copType arg =
    do xc <- C.getArg1 arg
       x  <- xc
       C.putType $ C.typeOf x

-- ----------------------  is, of, to

ofInfix :: C.CopCox c
ofInfix [f, x] = Right $ H.ix f [x]
ofInfix _ = Msg.adlib "require operand"

toInfix :: C.CopCox c
toInfix [x, f] = Right $ H.ix f [x]
toInfix _ = Msg.adlib "require operand"

-- ----------------------  if

nameLeaf :: B.BlankName -> B.TTree
nameLeaf = B.TreeL . B.TName B.codePtZero

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

