{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Cop
( vanillaContent,
  vanillaNamedContent,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Cop.Arith
import Koshucode.Baala.Vanilla.Cop.List
import Koshucode.Baala.Vanilla.Cop.Logic
import Koshucode.Baala.Vanilla.Cop.Order

import Koshucode.Baala.Vanilla.Value.Val



-- ----------------------

vanillaCop :: String -> Maybe (ContentOp Val)
vanillaCop n = lookup n $ concat [copOrder, copLogic, copArith, copList]

vanillaContent
    :: [SourceLine]       -- ^ Source information
    -> TokenTree          -- ^ Token tree of content formula
    -> AbortOr (PosContent Val)  -- ^ Partial content formula
vanillaContent src ts = 
    do c <- formContent vanillaCop src $ vanillaBinary ts
       Right $ posContent c

vanillaNamedContent
  :: [SourceLine]         -- ^ Source information
  -> Named TokenTree      -- ^ Token tree of content formula
  -> AbortOr (Named (PosContent Val))  -- ^ Partial content formula
vanillaNamedContent src (n, t) =
    do c <- vanillaContent src t
       Right (n, c)

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

