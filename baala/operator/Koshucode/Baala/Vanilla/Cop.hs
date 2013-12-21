{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaContent,
  vanillaNamedContent,
  vanillaNamedContents,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

import Koshucode.Baala.Vanilla.Cop.Arith
import Koshucode.Baala.Vanilla.Cop.List
import Koshucode.Baala.Vanilla.Cop.Literal
import Koshucode.Baala.Vanilla.Cop.Logic
import Koshucode.Baala.Vanilla.Cop.Order
import Koshucode.Baala.Vanilla.Type



-- ----------------------

vanillaCop :: C.FindCop VContent
vanillaCop n = lookup n ops where
    ops = concat [ copsArith
                 , copsLogic
                 , copsList
                 , copsLiteral
                 , copsOrder ]

vanillaContent
    :: C.RopUse VContent    -- ^ Source information
    -> B.TokenTree          -- ^ Token tree of content expression
    -> B.Ab (C.PosCox VContent) -- ^ Partial content expression
vanillaContent _ t =
    case C.formCox vanillaCop $ vanillaBinary t of
      Right c -> Right $ C.posCox c
      Left a  -> Left a

vanillaNamedContent
  :: C.RopUse VContent
  -> B.Named B.TokenTree
  -> B.Ab (B.Named (C.PosCox VContent))
vanillaNamedContent use (n, t) =
    do c <- vanillaContent use t
       Right (n, c)

vanillaNamedContents
  :: C.RopUse VContent
  -> [B.Named B.TokenTree]
  -> B.Ab [B.Named (C.PosCox VContent)]
vanillaNamedContents use = mapM (vanillaNamedContent use)

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

