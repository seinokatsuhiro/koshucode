{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaCox,
  vanillaNamedCox,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import qualified Koshucode.Baala.Vanilla.Cop.Arith    as V
import qualified Koshucode.Baala.Vanilla.Cop.List     as V
import qualified Koshucode.Baala.Vanilla.Cop.Literal  as V
import qualified Koshucode.Baala.Vanilla.Cop.Logic    as V
import qualified Koshucode.Baala.Vanilla.Cop.Order    as V
import qualified Koshucode.Baala.Vanilla.Type         as V

vanillaCop :: C.FindCop V.VContent
vanillaCop n = lookup n ops where
    ops = concat [ V.copsArith
                 , V.copsLogic
                 , V.copsList
                 , V.copsLiteral
                 , V.copsOrder ]

vanillaCox
    :: B.TokenTree                -- ^ Token tree of content expression
    -> B.Ab (C.CoxPos V.VContent) -- ^ Partial content expression
vanillaCox = C.coxPos vanillaCop . vanillaBinary

vanillaNamedCox :: B.Named B.TokenTree -> B.Ab (B.Named (C.CoxPos V.VContent))
vanillaNamedCox (name, tree) =
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

