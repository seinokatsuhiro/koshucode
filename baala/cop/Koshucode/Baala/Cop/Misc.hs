{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Misc
  ( copsMisc
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Cop.Coxhand       as H
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


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

-- | Content operators on miscellaneous.
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
copType :: (D.CType c, D.Basis c) => D.CopCalc c
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

nameLeaf :: S.BlankName -> S.Tree
nameLeaf = B.TreeL . S.TName B.def

treeIf :: S.Tree -> S.Tree -> S.Tree -> S.Tree
treeIf test con alt = S.ttreeGroup [ nameLeaf $ D.copInternal "#if" , test, con , alt ]

treeOrList :: [S.Tree] -> S.Tree
treeOrList [x] = x
treeOrList xs = S.ttreeGroup $ (nameLeaf $ D.copNormal "or") : xs

copFunIf  :: (D.CBool c, D.CEmpty c) => D.CopCalc c
copFunIf [D.getBool -> Right testB, conC, altC]
    | testB      = conC
    | otherwise  = altC
copFunIf cs = Msg.badArg cs

--  if TEST -> CON | ALT 
--  if TEST -> CON
--  if TEST -> CON | TEST -> CON | TEST -> CON
--  if | TEST -> CON | TEST -> CON | TEST -> CON

copTreeIf :: D.CopTree String
copTreeIf trees = folding $ B.omit null $ S.divideTreesBy (`elem` ["|", ":"]) trees where
    folding :: [[S.Tree]] -> B.Ab S.Tree
    folding []        = Right $ B.TreeL $ S.rawTextToken "()"
    folding (x : xs)  = fore x =<< folding xs

    fore :: [S.Tree] -> B.AbMap S.Tree
    fore trees2 alt =
        case S.divideTreesBy (== "->") trees2 of
          [_]         -> back trees2 alt
          [test, con] -> do test2 <- stairs ">>" "<<" test
                            Right $ treeIf test2 (S.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E -> E"

    back :: [S.Tree] -> B.AbMap S.Tree
    back trees2 alt =
        case S.divideTreesBy (== "<-") trees2 of
          [alt2]      -> Right $ S.ttreeGroup alt2
          [con, test] -> do test2 <- stairs "<<" ">>" test
                            Right $ treeIf test2 (S.ttreeGroup con) alt
          _           -> abortSyntax trees2 "Expect E <- E"

    stairs :: String -> String -> [S.Tree] -> B.Ab S.Tree
    stairs del del2 xs =
        do notInclude del2 xs
           Right $ treeOrList $ map S.ttreeGroup $ S.divideTreesBy (== del) xs

    notInclude :: String -> [S.Tree] -> B.Ab ()
    notInclude del xs =
        case S.divideTreesBy (== del) xs of
          [_] -> Right ()
          _   -> abortSyntax xs "Mixed arrows"

    abortSyntax xs msg =
        B.abortable "if" xs $ Msg.unexpAttr msg
