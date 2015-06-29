{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.About
  ( About (..),
    AboutJudges,
  ) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Data.Judge   as B

data About c = About [B.Named c] deriving (Show)
type AboutJudges c = (Maybe (About c), [B.Judge c])

