{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaContent,
  vanillaNamedContent,
  vanillaNamedContents,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap

import Koshucode.Baala.Vanilla.Cop.Arith
import Koshucode.Baala.Vanilla.Cop.List
import Koshucode.Baala.Vanilla.Cop.Literal
import Koshucode.Baala.Vanilla.Cop.Logic
import Koshucode.Baala.Vanilla.Cop.Order

import Koshucode.Baala.Vanilla.Value.Content



-- ----------------------

vanillaCop :: FindCop VContent
vanillaCop n = lookup n ops where
    ops = concat [ copArith
                 , copLogic
                 , copList
                 , copLiteral
                 , copOrder ]

vanillaContent
    :: OpUse VContent    -- ^ Source information
    -> TokenTree         -- ^ Token tree of content expression
    -> AbortOr (PosCox VContent)  -- ^ Partial content expression
vanillaContent use t =
    do let src = halfLines $ opHalf use
       case formCox vanillaCop $ vanillaBinary t of
         Right c -> Right $ posCox c
         Left a  -> Left (a, src)

vanillaNamedContent
  :: OpUse VContent
  -> Named TokenTree
  -> AbortOr (Named (PosCox VContent))
vanillaNamedContent use (n, t) =
    do c <- vanillaContent use t
       Right (n, c)

vanillaNamedContents
  :: OpUse VContent
  -> [Named TokenTree]
  -> AbortOr [Named (PosCox VContent)]
vanillaNamedContents use = mapM (vanillaNamedContent use)

{-| Convert infix form to prefix form. -}
vanillaBinary :: Map TokenTree
vanillaBinary = binaryTree ht where
    unbox (TWord _ 0 w) = w
    unbox _ = ""

    right n ws = (Right n, words ws)

    ht = heightTableUnbox unbox
         [ right 8 "or"
         , right 7 "and"
         , right 6 "= <> < > <= >="
         , right 2 "+ -"
         , right 1 "* /"
         ]

