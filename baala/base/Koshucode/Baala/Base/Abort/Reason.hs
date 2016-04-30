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

import qualified Koshucode.Baala.Base.Text  as B

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

--  HTML
--
--    Aborted
--
--    {main reason}
--      {detailed reason}
--      {detailed reason}
--
--    {line #} {col #} {filename}
--      {line text}
--    {line #} {col #} {filename}
--      {line text}

instance B.ToMarkup AbortReason where
    toMarkup a =
        B.div_ "abort" $ do
          B.div_ "abort-title"   $ text "Aborted"
          B.div_ "abort-reason"  $ text $ abortReason a
          B.div_ "abort-detail"  $ mapM_ detail  $ abortDetail a
          B.div_ "abort-source"  $ mapM_ code $ abortPoint a
        where
          text n         = B.toMarkup (n :: String)
          detail x       = B.div $ B.toMarkup x
          code (ctx, p)  = point p ctx >> line p
          line p         = B.div_ "abort-line" $ text $ B.codePtText p
          source k n c   = B.span_ k $ text n >> (B.toMarkup c)
          point p ctx    = B.div_ "abort-point" $ do
                                  source "abort-point-line"    "Line "    $ B.codePtLineNo p
                                  source "abort-point-column"  "Column "  $ B.codePtColumnNo p
                                  source "abort-point-context" "Context " ctx
