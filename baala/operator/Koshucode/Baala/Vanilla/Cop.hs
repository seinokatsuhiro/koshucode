{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaCops,
  vanillaCox,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import qualified Koshucode.Baala.Vanilla.Type         as Rop
import qualified Koshucode.Baala.Vanilla.Cop.Arith    as Cop
import qualified Koshucode.Baala.Vanilla.Cop.List     as Cop
import qualified Koshucode.Baala.Vanilla.Cop.Logic    as Cop
import qualified Koshucode.Baala.Vanilla.Cop.Order    as Cop

{-| List of term-content operators. -}
vanillaCops :: [C.Cop Rop.VContent]
vanillaCops = concat ops where
    ops = [ Cop.copsArith
          , Cop.copsLogic
          , Cop.copsList
          , Cop.copsOrder ]

vanillaCox :: B.TokenTree -> B.Ab (C.CoxCons Rop.VContent)
vanillaCox = C.coxCons (`lookup` assoc) . vanillaBinary where
    assoc = map B.named vanillaCops

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

