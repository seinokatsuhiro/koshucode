{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Core.Relmap.Assert
( -- * Datatype
  Assert (..)
, assertMap

  -- * Constructors
, affirm
, deny
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Relmap.Relmap

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap.

    See also 'Judge' -}
data Assert v = Assert
    { assertQuality :: Bool       -- ^ Logical quality
    , assertRelsign :: B.Relsign  -- ^ Sign of relation
    , assertRelmap  :: Relmap v   -- ^ Relmap
    } deriving (Show)

instance B.Pretty (Assert v) where
    doc (Assert q s m) =
        let qs = B.text (verb q) B.<+> B.text s
        in B.hang qs 2 (B.doc m)

verb :: Bool -> String
verb True  = "affirm"
verb False = "deny"

{-| Apply function to relamp in assert. -}
assertMap :: B.Map (Relmap v) -> B.Map (Assert v)
assertMap f (Assert q s r) = Assert q s $ f r

{-| Make affirmed assertion. -}
affirm :: B.Relsign -> Relmap v -> Assert v
affirm = Assert True

{-| Make denied assertion. -}
deny :: B.Relsign -> Relmap v -> Assert v
deny = Assert False

