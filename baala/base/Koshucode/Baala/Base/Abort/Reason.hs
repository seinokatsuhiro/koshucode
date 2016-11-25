{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons.

module Koshucode.Baala.Base.Abort.Reason
  ( -- * Data type
    AbortReason (..),
    AbortTag,

    -- * Derived type
    Ab, AbMap, MapAb, AbManyMap,
    AbTest, IOAb, BinAb,
  
    -- * Creation
    abortBecause,
    abortLine, abortLines,
    abortPage,

    -- * Alteration
    Abortable,
    abortable,
  ) where

import qualified Koshucode.Baala.Overture   as O
import qualified Koshucode.Baala.Base.IO    as B


-- --------------------------------------------  Type

-- | Abort reason.
data AbortReason = AbortReason
    { abortReason :: String       -- ^ Reason in one line
    , abortDetail :: [String]     -- ^ Detailed description
    , abortNote   :: [String]     -- ^ Additional notes for long description
    , abortPoint  :: [(AbortTag, B.CodePos)] -- ^ Tag and aborting point
    } deriving (Show, Eq, Ord)

-- | Tag on aborting point.
type AbortTag = String


-- --------------------------------------------  Deriving type

-- | Abortable result, i.e., either of right result or abort reason.
type Ab a = Either AbortReason a

-- | Abortable mapping.
type AbMap a = a -> Ab a

-- | Abortable-to-abortable mapping.
type MapAb a = Ab a -> Ab a

-- | Abortable many map.
type AbManyMap a = a -> Ab [a]

-- | Abortable Boolean-valued function.
type AbTest a = a -> Ab Bool

-- | Abortable I/O.
type IOAb a = IO (Ab a)

-- | Type for abortable binary operators.
type BinAb a = a -> a -> Ab a


-- --------------------------------------------  Creation

-- | Construct abort reason with reason text.
abortBecause :: String -> AbortReason
abortBecause r = AbortReason r [] [] []

-- | Construct abort reason with reason and detailed text.
abortLine :: String -> String -> AbortReason
abortLine  r d = AbortReason r [d] [] []

-- | Construct abort reason with reason and multilined detailed text.
abortLines :: String -> [String] -> AbortReason
abortLines r d = AbortReason r d [] []

-- | Construct abort reason with reason and note.
abortPage :: String -> [String] -> AbortReason
abortPage  r n = AbortReason r [] n []


-- --------------------------------------------  Alteration

-- | Abortable process.
type Abortable cp b = cp -> MapAb b

-- | Push source information when process is aborted.
abortable :: (B.CodePtr cp) => AbortTag -> Abortable cp b
abortable tag cp = either (Left . push tag (B.codePtList cp)) Right

push :: AbortTag -> [B.CodePos] -> O.Map AbortReason
push _ [] a = a
push tag (p:_) a@AbortReason { abortPoint = ps } =
    a { abortPoint = (tag, p) : ps }

