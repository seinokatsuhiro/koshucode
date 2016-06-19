{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abort reasons.

module Koshucode.Baala.Base.Abort.Reason
  ( -- * Data type
    AbortReason (..),
    Ab, AbMap, AbManyMap, AbPred, IOAb, BinAb,
  
    -- * Constructor
    abortBecause,
    abortLine, abortLines,
    abortPage,
  ) where

import qualified Koshucode.Baala.Base.IO    as B

data AbortReason = AbortReason
    { abortReason :: String                -- ^ Reason in one line
    , abortDetail :: [String]              -- ^ Detailed description
    , abortNote   :: [String]              -- ^ Additional notes for long description
    , abortPoint  :: [(String, B.CodePt)]  -- ^ Tag and aborting point
    } deriving (Show, Eq, Ord)

-- | Abortable result, i.e., either of right result or abort reason.
type Ab a = Either AbortReason a

-- | Abortable mapping.
type AbMap a = a -> Ab a

type AbManyMap a = a -> Ab [a]

type AbPred a = a -> Ab Bool

type IOAb a = IO (Ab a)

-- | Type for abortable binary operators.
type BinAb a = a -> a -> Ab a

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

