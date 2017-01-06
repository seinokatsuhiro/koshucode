{-# OPTIONS_GHC -Wall #-}

-- | Value of some types.

module Koshucode.Baala.Overture.Value
 ( Value (..),
 ) where

import Data.Text         as Tx
import Data.Text.Lazy    as Tz

-- | Value of some types.
data Value
    = VEmpty            -- ^ Empty
    | VBool Bool        -- ^ Boolean
    | VInt Int          -- ^ Integer
    | VInteger Integer  -- ^ Integer
    | VStr String       -- ^ Text
    | VTx Tx.Text       -- ^ Strict text
    | VTz Tz.Text       -- ^ Lazy text
    | VList [Value]     -- ^ List of values
      deriving (Show, Eq, Ord)

