{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Core.Assert.Assert
  ( -- * Data type
    Assert' (..),
  
    -- * Short assertion
    ShortAssert', ShortAsserts',
    assertNormal, assertViolated,
  ) where

import qualified Koshucode.Baala.Overture       as O
import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Syntax         as S
import qualified Koshucode.Baala.Data           as D
import qualified Koshucode.Baala.Core.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap    as C


-- ----------------------  Assert

-- | Affirming or denying relation.
--   It consists of logical quality, judgement pattern, and relmap.
--   See also 'B.Judge'
data Assert' h c = Assert
    { assSection   :: C.SecNo                -- ^ Section number
    , assType      :: D.AssertType           -- ^ Logical quality
    , assClass     :: D.JudgeClass           -- ^ Judgement class
    , assToken     :: [S.Token]              -- ^ Source token list
    , assPara      :: C.TTreePara
    , assRelmap    :: Maybe (C.Relmap' h c)  -- ^ Relmap
    , assLinks     :: C.RelmapLinkTable' h c
    } deriving (Show)

instance B.CodePtr (Assert' h c) where
    codePtList = concatMap B.codePtList . assToken


-- ----------------------  Short assertion

-- | Assertion with short signs.
type ShortAssert' h c = S.Short (Assert' h c)

-- | Assertion list with short signs.
type ShortAsserts' h c = S.Short [Assert' h c]

-- | Select affirmative or denial assertions.
assertNormal :: O.Map [ShortAssert' h c]
assertNormal = B.omit violated

-- | Select violated assertions.
assertViolated :: O.Map [ShortAssert' h c]
assertViolated = filter violated

violated :: ShortAssert' h c -> Bool
violated = (== D.AssertViolate) . assType . S.shortBody

