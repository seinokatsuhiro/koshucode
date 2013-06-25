{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Base.Relmap.Assert
( -- * Datatype
  Assert (..)
, assertMap

  -- * Constructors
, affirm
, deny
) where

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Relmap.Relmap

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap.

    See also 'Judge' -}
data Assert v = Assert
    { assertQuality :: Bool      -- ^ Logical quality
    , assertRelsign :: Relsign   -- ^ Sign of relation
    , assertRelmap  :: Relmap v  -- ^ Relmap
    } deriving (Show)

instance Pretty (Assert v) where
    doc (Assert q s m) =
        let qs = text (verb q) <+> text s
        in hang qs 2 (doc m)

verb :: Bool -> String
verb True  = "affirm"
verb False = "deny"

{-| Apply function to relamp in assert. -}
assertMap :: Map (Relmap v) -> Map (Assert v)
assertMap f (Assert q s r) = Assert q s $ f r

{-| Make affirmed assertion. -}
affirm :: Relsign -> Relmap v -> Assert v
affirm = Assert True

{-| Make denied assertion. -}
deny :: Relsign -> Relmap v -> Assert v
deny = Assert False

