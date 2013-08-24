{-# OPTIONS_GHC -Wall #-}

{-| Data structure for mapping relation to judges -}

module Koshucode.Baala.Core.Relmap.Assert
( -- * Datatype
  Assert (..),
  AssertOption,
  assertMap,

  -- * Constructors
  affirm,
  deny,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Relmap.Relmap

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap.

    See also 'Judge' -}
data Assert c = Assert
    { assertQuality :: Bool            -- ^ Logical quality
    , assertPattern :: B.JudgePattern  -- ^ Pattern of judgement
    , assertOption  :: AssertOption    -- ^ Assert option
    , assertRelmap  :: Relmap c        -- ^ Relmap
    , assertSource  :: [B.TokenLine]   -- ^ Source code information
    } deriving (Show)

{-| Option for @affirm@ or @deny@. -}
type AssertOption = [B.Named [B.TokenTree]]

instance B.Pretty (Assert c) where
    doc (Assert q pat _ r _) =
        let qs = B.doch [quality q, pat]
        in B.docHang qs 2 (B.doc r)

quality :: Bool -> String
quality True  = "affirm"
quality False = "deny"

{-| Apply function to relamp in assert. -}
assertMap :: B.Map (Relmap c) -> B.Map (Assert c)
assertMap f (Assert q pat opt r src) = Assert q pat opt (f r) src

{-| Make affirmed assertion. -}
affirm :: B.JudgePattern -> AssertOption -> Relmap c -> [B.TokenLine] -> Assert c
affirm = Assert True

{-| Make denied assertion. -}
deny :: B.JudgePattern -> AssertOption -> Relmap c -> [B.TokenLine] -> Assert c
deny = Assert False

