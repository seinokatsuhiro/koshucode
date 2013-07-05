{-# OPTIONS_GHC -Wall #-}

-- When you use ghci, please :set -idist/build/autogen

module Koshucode.Baala.Toolkit.Library.Version
( versionString
) where

import qualified Data.Version as Version
import qualified Paths_koshucode_baala_processor as Version

versionString :: String
versionString = Version.showVersion Version.version

