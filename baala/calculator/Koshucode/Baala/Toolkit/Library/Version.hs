{-# OPTIONS_GHC -Wall #-}

-- When you use ghci, please :set -idist/build/autogen

-- | Version number.
module Koshucode.Baala.Toolkit.Library.Version
  ( version,
    versionString,
  ) where

import qualified Data.Version                     as V
import qualified Paths_koshucode_baala_calculator as V

-- | Version number of the Koshucode calculator.
version :: V.Version
version = V.version

-- | String representation of the version number.
versionString :: String
versionString = V.showVersion V.version

