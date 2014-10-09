{-# OPTIONS_GHC -Wall #-}

-- | Term names in relation.

module Koshucode.Baala.Base.Data.Term
(
  Term (..),
)
where

import qualified Koshucode.Baala.Base.Token as B

data Term
    = TermFlat B.TermName          -- ^ Term name for non-relation
    | TermNest B.TermName [Term]   -- ^ Term name for relation
      deriving (Show, Eq, Ord)

