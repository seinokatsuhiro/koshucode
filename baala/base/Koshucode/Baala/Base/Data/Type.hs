{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Type
( Type (..),
) where

import qualified Koshucode.Baala.Base.Token   as B

data Type
    = TypeEmpty
    | TypeBool
    | TypeText
    | TypeRel   [(B.TermName, Type)]
    | TypeSum   [Type]
      deriving (Show, Eq, Ord)

