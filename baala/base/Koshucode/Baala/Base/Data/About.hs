{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.About
  ( About (..),
    AboutJudges,
    writeDownAbout,
  ) where

import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Judge   as B

data About c = About [B.Term c] deriving (Show)
type AboutJudges c = (Maybe (About c), [B.Judge c])

writeDownAbout :: (B.Write c) => B.StringMap -> About c -> String
writeDownAbout sh (About xs) = "about" ++ B.writeDownTerms sh xs

