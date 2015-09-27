{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.About
  ( About (..),
    AboutJudges,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Token        as D
import qualified Koshucode.Baala.Data.Type.Judge   as D

data About c = About [D.Term c] deriving (Show)
type AboutJudges c = (Maybe (About c), [D.Judge c])

instance (B.Write c) => B.Write (About c) where
    writeDocWith sh (About xs) = B.doc "about" B.<> B.doc (D.writeDownTerms sh xs)
