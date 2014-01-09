{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaCops,
  vanillaCox,
  vanillaCoxNamed,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import qualified Koshucode.Baala.Vanilla.Cop.Arith    as V
import qualified Koshucode.Baala.Vanilla.Cop.List     as V
import qualified Koshucode.Baala.Vanilla.Cop.Literal  as V
import qualified Koshucode.Baala.Vanilla.Cop.Logic    as V
import qualified Koshucode.Baala.Vanilla.Cop.Order    as V
import qualified Koshucode.Baala.Vanilla.Type         as V

vanillaCops :: [B.Named (C.Cop V.VContent)]
vanillaCops = concat [ V.copsArith
                     , V.copsLogic
                     , V.copsList
                     , V.copsLiteral
                     , V.copsOrder ]

vanillaCox :: B.TokenTree -> B.Ab (C.CoxCons V.VContent)
vanillaCox = C.coxCons (`lookup` vanillaCops) . vanillaBinary

vanillaCoxNamed :: B.Named B.TokenTree -> B.Ab (B.Named (C.CoxCons V.VContent))
vanillaCoxNamed (name, tree) =
    do cox <- vanillaCox tree
       Right (name, cox)

{-| Convert infix form to prefix form. -}
vanillaBinary :: B.Map B.TokenTree
vanillaBinary = B.binaryTree ht where
    unbox (B.TWord _ 0 w) = w
    unbox _ = ""

    right n ws = (Right n, words ws)

    ht = B.heightTableUnbox unbox
         [ right 9 "then when unless"
         , right 8 "or"
         , right 7 "and"
         , right 6 "= <> < > <= >="
         , right 2 "+ - ++ intersect minus"
         , right 1 "* / quo rem"
         ]

