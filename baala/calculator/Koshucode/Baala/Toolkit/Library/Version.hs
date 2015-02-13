{-# OPTIONS_GHC -Wall #-}

-- When you use ghci, please :set -idist/build/autogen

module Koshucode.Baala.Toolkit.Library.Version
  ( version,
    versionString,
  ) where

import qualified Data.Version as V
import qualified Paths_koshucode_baala_calculator as V

version :: V.Version
version = V.version

versionString :: String
versionString = V.showVersion V.version

