{-# OPTIONS_GHC -Wall #-}

{-| Data structure for mapping relation to judges -}

module Koshucode.Baala.Core.Assert.Assert
( -- * Datatype
  Assert (..),
  AssertOption,
  assertMap,

  -- * Constructors
  affirm,
  deny,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap as C

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap.

    See also 'Judge' -}
data Assert c = Assert
    { assertQuality :: Bool            -- ^ Logical quality
    , assertPattern :: B.JudgePattern  -- ^ Pattern of judgement
    , assertOption  :: AssertOption    -- ^ Assert option
    , assertRelmap  :: C.Relmap c      -- ^ Relmap
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
assertMap :: B.Map (C.Relmap c) -> B.Map (Assert c)
assertMap f (Assert q pat opt r src) = Assert q pat opt (f r) src

affirm, deny :: B.JudgePattern -> AssertOption
             -> C.Relmap c -> [B.TokenLine] -> Assert c

{-| Make affirmed assertion. -}
affirm = Assert True

{-| Make denied assertion. -}
deny = Assert False

