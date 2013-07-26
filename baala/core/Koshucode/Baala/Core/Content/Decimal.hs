{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Decimal
( LitString,
  readDecimal,  
) where

import Koshucode.Baala.Base

{-| Make @a@ from a string. -}
type LitString a = AbMap2 String a

readDecimal :: LitString Int
readDecimal s =
    case reads s of
      [(n, "")] -> Right n
      _         -> Left $ AbortNotNumber s


