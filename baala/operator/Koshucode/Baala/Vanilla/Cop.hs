{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaContent,
  vanillaNamedContent,
  vanillaNamedContents,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Relmap

import Koshucode.Baala.Vanilla.Cop.Arith
import Koshucode.Baala.Vanilla.Cop.List
import Koshucode.Baala.Vanilla.Cop.Logic
import Koshucode.Baala.Vanilla.Cop.Order

import Koshucode.Baala.Vanilla.Value.Content



-- ----------------------

vanillaCop :: String -> Maybe (ContentOp VContent)
vanillaCop n = lookup n $ concat [copOrder, copLogic, copArith, copList]

vanillaContent
    :: OpUse VContent         -- ^ Source information
    -> TokenTree         -- ^ Token tree of content formula
    -> AbortOr (PosContent VContent)  -- ^ Partial content formula
vanillaContent use ts = 
    do let src = halfLines $ opHalf use
       c <- formContent vanillaCop src $ vanillaBinary ts
       Right $ posContent c

vanillaNamedContent
  :: OpUse VContent
  -> Named TokenTree
  -> AbortOr (Named (PosContent VContent))
vanillaNamedContent use (n, t) =
    do c <- vanillaContent use t
       Right (n, c)

vanillaNamedContents
  :: OpUse VContent
  -> [Named TokenTree]
  -> AbortOr [Named (PosContent VContent)]
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

