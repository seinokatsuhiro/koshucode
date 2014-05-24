{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Core.Assert.Assert
( -- * Data type
  Assert (..),
  AssertType (..),
  AssertOption,
  assertQuality,

  -- * Short assertion
  ShortAssert,
  assertNormal,
  assertViolated,
) where

import qualified Data.Generics               as G
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core.Lexmap as C
import qualified Koshucode.Baala.Core.Relmap as C



-- ----------------------  Assert

-- | Affirming or denying relation.
--   It consists of logical quality, judgement pattern, and relmap.
--   See also 'B.Judge'
data Assert c = Assert
    { assType    :: AssertType          -- ^ Logical quality
    , assPattern :: B.JudgePat          -- ^ Pattern of judgement
    , assOption  :: AssertOption        -- ^ Assert option
    , assToken   :: [B.Token]           -- ^ Source token list
    , assTree    :: [B.TokenTree]       -- ^ Token relmap
    , assRelmap  :: Maybe (C.Relmap c)  -- ^ Relmap
    , assParts   :: [C.Roal (C.Relmap c)]
    } deriving (Show)

-- | Option for assertions.
type AssertOption = [B.NamedTrees]

instance B.CodePointer (Assert c) where
    codePoint = concatMap B.codePoint . assToken

instance B.Write (Assert c) where
    write sh (Assert q pat _ toks _ _ _) =
        let qs = B.writeH sh [assertText q, pat]
        in B.docHang qs 2 (B.writeH sh toks)

data AssertType
    = AssertAffirm    -- ^ @|==@ /pattern/ @:@ /relmap/
    | AssertDeny      -- ^ @|=X@ /pattern/ @:@ /relmap/
    | AssertViolate   -- ^ @|=V@ /pattern/ @:@ /relmap/
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

assertQuality :: AssertType -> Bool
assertQuality AssertAffirm   = True
assertQuality AssertDeny     = False
assertQuality AssertViolate  = True

assertText :: AssertType -> String
assertText AssertAffirm   = "|=="
assertText AssertDeny     = "|=X"
assertText AssertViolate  = "|=V"


-- ----------------------  Short assertion

-- | Assertion list with short signs.
type ShortAssert c = B.Short [Assert c]

-- | Select affirmative or denial assertions.
assertNormal :: B.Map [ShortAssert c]
assertNormal = B.shortMap $ B.omit violated

-- | Select violated assertions.
assertViolated :: B.Map [ShortAssert c]
assertViolated = B.shortMap $ filter violated

violated :: Assert c -> Bool
violated = (== AssertViolate) . assType

