{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Misc
  ( copsMisc
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S
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

copsMisc :: (D.CContent c) => [D.Cop c]
copsMisc =
    [ D.CopCalc  (D.copNormal   "type")    copType
    , D.CopCalc  (D.copNormal   "fill")    copFill

    , D.CopCox   (D.copInfix    "is")      toInfix
    , D.CopCox   (D.copInfix    "of")      ofInfix
    , D.CopCox   (D.copInfix    "to")      toInfix

    , D.CopCalc  (D.copInternal "#if")     copFunIf
    , D.CopTree  (D.copNormal   "if")      copTreeIf
    ]


-- ----------------------  type

-- /input [- X -]  /output [- type -]
copType :: (D.CType c, D.CTypeOf c) => D.CopCalc c
copType arg =
    do x <- D.getRightArg1 arg
       D.putType $ D.typeOf x

-- ----------------------  fill

-- /input [- X -] [- X | empty -] ...  /output [- X -]
copFill :: (D.CContent c) => D.CopCalc c
copFill = op . reverse where
    op [] = Right D.empty
    op (c' : cs') = do c <- c'
                       case D.isEmpty c of
                         False -> Right c
                         True  -> op cs'

-- ----------------------  is, of, to

ofInfix :: D.CopCox c
ofInfix [f, x] = Right $ H.ix f [x]
ofInfix _ = Msg.adlib "require operand"

toInfix :: D.CopCox c
toInfix [x, f] = Right $ H.ix f [x]
toInfix _ = Msg.adlib "require operand"

-- ----------------------  if

nameLeaf :: S.BlankName -> S.TTree
nameLeaf = B.TreeL . S.TName B.def

treeIf :: S.TTree -> S.TTree -> S.TTree -> S.TTree
treeIf test con alt = S.ttreeGroup [ nameLeaf $ D.copInternal "#if" , test, con , alt ]

treeOrList :: [S.TTree] -> S.TTree
treeOrList [x] = x
treeOrList xs = S.ttreeGroup $ (nameLeaf $ D.copNormal "or") : xs

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
copTreeIf trees = folding $ filter (/= []) $ S.divideTreesByColon trees where
    folding :: [[S.TTree]] -> B.Ab S.TTree
    folding []        = Right $ B.TreeL $ S.textToken "()"
    folding (x : xs)  = fore x =<< folding xs

    fore :: [S.TTree] -> B.AbMap S.TTree
    fore trees2 alt =
        case S.divideTreesBy (== "->") trees2 of
          [_]         -> back trees2 alt
          [test, con] -> do test2 <- stairs ">>" "<<" test
                            Right $ treeIf test2 (S.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E -> E"

    back :: [S.TTree] -> B.AbMap S.TTree
    back trees2 alt =
        case S.divideTreesBy (== "<-") trees2 of
          [alt2]      -> Right $ S.ttreeGroup alt2
          [con, test] -> do test2 <- stairs "<<" ">>" test
                            Right $ treeIf test2 (S.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E <- E"

    stairs :: String -> String -> [S.TTree] -> B.Ab S.TTree
    stairs del del2 xs =
        do notInclude del2 xs
           Right $ treeOrList $ map S.ttreeGroup $ S.divideTreesBy (== del) xs

    notInclude :: String -> [S.TTree] -> B.Ab ()
    notInclude del xs =
        case S.divideTreesBy (== del) xs of
          [_] -> Right ()
          _   -> abortSyntax xs "Mixed arrows"

    abortSyntax xs msg =
        Msg.abortableTrees "if" xs $
         Msg.unexpAttr msg

