{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Literal
( copsLiteral
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type



-- ----------------------
{- $Operators

 [@\'@]      Text.

-}

litText :: [B.TokenTree] -> B.AbOr VContent
litText xs =
    do ss <- mapM litT xs
       Right . C.putText $ concat ss

litT :: B.TokenTree -> B.AbOr String
litT (B.TreeL (B.TWord _ _ w)) = Right w
litT x = Left $ B.AbortNotText (show x)

copsLiteral :: [B.Named (C.Cop VContent)]
copsLiteral =
    [ C.namedLit    "'"       litText
    ]

