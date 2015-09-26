{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.About
  ( About (..),
    AboutJudges,
  ) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Data.Token        as B
import qualified Koshucode.Baala.Data.Type.Judge   as B

data About c = About [B.Term c] deriving (Show)
type AboutJudges c = (Maybe (About c), [B.Judge c])

instance (B.Write c) => B.Write (About c) where
    writeDocWith sh (About xs) = B.doc "about" B.<> B.doc (B.writeDownTerms sh xs)
