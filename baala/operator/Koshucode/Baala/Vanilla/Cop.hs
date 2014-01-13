{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaCops,
  vanillaHeightTable,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import qualified Koshucode.Baala.Vanilla.Type      as Type
import qualified Koshucode.Baala.Vanilla.Cop.Arith as Cop
import qualified Koshucode.Baala.Vanilla.Cop.List  as Cop
import qualified Koshucode.Baala.Vanilla.Cop.Logic as Cop
import qualified Koshucode.Baala.Vanilla.Cop.Order as Cop

{-| List of term-content operators. -}
vanillaCops :: [C.Cop Type.VContent]
vanillaCops = concat ops where
    ops = [ Cop.copsArith
          , Cop.copsLogic
          , Cop.copsList
          , Cop.copsOrder ]

vanillaHeightTable :: [B.Named B.InfixHeight]
vanillaHeightTable =
    [ 9 ! "then"
    , 9 ! "when"
    , 9 ! "unless"

    , 8 ! "or"

    , 7 ! "and"

    , 6 ! "="
    , 6 ! "<>"
    , 6 ! "<"
    , 6 ! ">"
    , 6 ! "<="
    , 6 ! ">="

    , 2 ! "+"
    , 2 ! "-"
    , 2 ! "++"
    , 2 ! "intersect"
    , 2 ! "minus"

    , 1 ! "*"
    , 1 ! "/"
    , 1 ! "quo"
    , 1 ! "rem"
    ]
    where h ! name = (name, Right h)
    

