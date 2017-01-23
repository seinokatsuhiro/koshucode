{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons.

module Koshucode.Baala.Base.Abort.Reason
  ( -- * Data type
    AbortReason (..),
    AbortTag,
    CodePosInfo,
    abortPointUp,

    -- * Derived type
    Ab, AbMap, MapAb, AbManyMap,
    AbTest, IOAb, BinAb,
  
    -- * Creation
    abortBecause, abortLine, abortLines, abortPage,
    leftBecause, leftLine, leftLines, leftPage,

    -- * Alteration
    Abortable,
    abortable,
    unabort,
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base.List            as B
import qualified Koshucode.Baala.Base.Abort.CodePos   as B


-- --------------------------------------------  Type

-- | Abort reason.
data AbortReason = AbortReason
    { abortReason :: String        -- ^ Reason in one line
    , abortDetail :: [String]      -- ^ Detailed description
    , abortNote   :: [String]      -- ^ Additional notes for long description
    , abortPoint  :: [CodePosInfo] -- ^ Tag and aborting points
    } deriving (Show, Eq, Ord)

-- | Tag on aborting point.
type AbortTag = String

-- | Source code position information.
type CodePosInfo = (B.CodePos, AbortTag)

-- | Upward aborting points, i.e., from callee to caller.
abortPointUp :: AbortReason -> [CodePosInfo]
abortPointUp = reverse . B.unique . abortPoint


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

tReason :: (O.Textual s, O.Textual t) => s -> [t] -> [t] -> [CodePosInfo] -> AbortReason
tReason r ds ns = AbortReason r' ds' ns' where
    r'  = O.tString r
    ds' = O.tString <$> ds
    ns' = O.tString <$> ns

-- | Construct abort reason with reason text.
abortBecause :: (O.Textual s) => s -> AbortReason
abortBecause r = tReason r ([] :: [String]) [] []

-- | Construct abort reason with reason and detailed text.
abortLine :: (O.Textual s, O.Textual t) => s -> t -> AbortReason
abortLine r d = abortLines r [d]

-- | Construct abort reason with reason and multilined detailed text.
abortLines :: (O.Textual s, O.Textual t) => s -> [t] -> AbortReason
abortLines r ds = tReason r ds [] []

-- | Construct abort reason with reason and note.
abortPage :: (O.Textual s, O.Textual t) => s -> [t] -> AbortReason
abortPage r ns = tReason r [] ns []

-- | 'Left' plus 'abortBecause'.
leftBecause :: (O.Textual s) => s -> Ab a
leftBecause r = Left $ abortBecause r

-- | 'Left' plus 'abortLine'.
leftLine :: (O.Textual s, O.Textual t) => s -> t -> Ab a
leftLine r d = Left $ abortLine r d

-- | 'Left' plus 'abortLines'.
leftLines :: (O.Textual s, O.Textual t) => s -> [t] -> Ab a
leftLines r ds = Left $ abortLines r ds

-- | 'Left' plus 'abortPage'.
leftPage :: (O.Textual s, O.Textual t) => s -> [t] -> Ab a
leftPage r ns = Left $ abortPage r ns


-- --------------------------------------------  Alteration

-- | Abortable process.
type Abortable cp b = cp -> MapAb b

-- | Push source information when process is aborted.
abortable :: (B.GetCodePos cp) => AbortTag -> Abortable cp b
abortable tag cp = either (Left . push tag (B.getCPs cp)) Right

push :: AbortTag -> [B.CodePos] -> O.Map AbortReason
push _ [] a = a
push tag (cp : _) a@AbortReason { abortPoint = ps } =
    a { abortPoint = (cp, tag) : ps }

-- | Extract right value or print error message.
unabort :: Ab a -> a
unabort (Right a)  = a
unabort (Left a)   = error $ abortReason a
