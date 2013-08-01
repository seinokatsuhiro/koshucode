{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Literal
( copLiteral
-- $Operators
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core

import Koshucode.Baala.Vanilla.Type.Content



-- ----------------------
{- $Operators

 [@\'@]      Text.

-}

litText :: [TokenTree] -> AbOr VContent
litText xs =
    do ss <- mapM litT xs
       Right . putText $ concat ss

litT :: TokenTree -> AbOr String
litT (TreeL (TWord _ _ w)) = Right w
litT x = Left $ AbortNotText (show x)

copLiteral :: [Named (Cop VContent)]
copLiteral =
    [ namedLit    "'"       litText
    ]

