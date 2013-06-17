{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Base.Struct.Full.Assert
( Assert (..)
, affirm
, deny
) where

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Struct.Full.Relmap

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap. -}
data Assert v = Assert
    { assertQuality :: Bool        -- ^ Logical quality
    , assertRelsign :: Relsign     -- ^ Sign of relation
    , assertRelmap  :: Relmap v    -- ^ Relmap
    } deriving (Show)

instance Pretty (Assert v) where
    doc (Assert q s m) =
        let qs = text (verb q) <+> text s
        in hang qs 2 (doc m)

verb :: Bool -> String
verb True  = "affirm"
verb False = "deny"

{-| Make affirmed assertion. -}
affirm :: Relsign -> Relmap v -> Assert v
affirm = Assert True

{-| Make denied assertion. -}
deny :: Relsign -> Relmap v -> Assert v
deny = Assert False

