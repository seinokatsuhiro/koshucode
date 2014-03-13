{-# OPTIONS_GHC -Wall #-}

-- | Term-content operators.

module Koshucode.Baala.Op.Vanilla.Cop
( vanillaCops,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import qualified Koshucode.Baala.Op.Vanilla.Type      as Type
import qualified Koshucode.Baala.Op.Vanilla.Cop.Arith as Op
import qualified Koshucode.Baala.Op.Vanilla.Cop.List  as Op
import qualified Koshucode.Baala.Op.Vanilla.Cop.Logic as Op
import qualified Koshucode.Baala.Op.Vanilla.Cop.Order as Op

-- | Term-content operators and its height table.
vanillaCops :: ([C.Cop Type.VContent], [B.Named B.InfixHeight])
vanillaCops = (concat cops, htab) where

    cops = [ Op.copsArith
           , Op.copsLogic
           , Op.copsList
           , Op.copsOrder ]

    h ! name = (name, Right h)

    htab =
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

        , ("<left-1>"  , Left 1)
        , ("<right-1>" , Right 1)
        ]
    
