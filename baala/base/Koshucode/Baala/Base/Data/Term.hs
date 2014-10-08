{-# OPTIONS_GHC -Wall #-}

-- | Term names in relation.

module Koshucode.Baala.Base.Data.Term
(
  Term (..),
  termChange,
)
where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B

data Term
    = TermFlat B.TermName          -- ^ Term name for non-relation
    | TermNest B.TermName [Term]   -- ^ Term name for relation
      deriving (Show, Eq, Ord)

termChange :: B.Map B.TermName -> B.Map Term
termChange f (TermFlat n)    = TermFlat (f n)
termChange f (TermNest n ts) = TermNest (f n) ts

