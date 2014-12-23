{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Core.Assert.Assert
  ( -- * Data type
    Assert (..),
    AssertOption,
  
    -- * Short assertion
    ShortAssert,
    assertNormal,
    assertViolated,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Core.Content   as C
import qualified Koshucode.Baala.Core.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap    as C


-- ----------------------  Assert

-- | Affirming or denying relation.
--   It consists of logical quality, judgement pattern, and relmap.
--   See also 'B.Judge'
data Assert h c = Assert
    { assSection  :: C.SecNo               -- ^ Section number
    , assType     :: C.AssertType          -- ^ Logical quality
    , assPattern  :: B.JudgePat            -- ^ Pattern of judgement
    , assOption   :: AssertOption          -- ^ Assert option
    , assToken    :: [B.Token]             -- ^ Source token list
    , assTree     :: [B.TTree]             -- ^ Token relmap
    , assRelmap   :: Maybe (C.Relmap h c)  -- ^ Relmap
    , assLinks    :: C.RelmapLinkTable h c
    } deriving (Show)

-- | Option for assertions.
type AssertOption = [B.NamedTrees]

instance B.CodePtr (Assert h c) where
    codePts = concatMap B.codePts . assToken

instance B.Write (Assert h c) where
    write sh (Assert _ q pat _ toks _ _ _) =
        let qs = B.writeH sh [C.assertSymbol q, pat]
        in B.docHang qs 2 (B.writeH sh toks)


-- ----------------------  Short assertion

-- | Assertion list with short signs.
type ShortAssert h c = B.Short [Assert h c]

-- | Select affirmative or denial assertions.
assertNormal :: B.Map [ShortAssert h c]
assertNormal = B.map2 $ B.omit violated

-- | Select violated assertions.
assertViolated :: B.Map [ShortAssert h c]
assertViolated = B.map2 $ filter violated

violated :: Assert h c -> Bool
violated = (== C.AssertViolate) . assType

