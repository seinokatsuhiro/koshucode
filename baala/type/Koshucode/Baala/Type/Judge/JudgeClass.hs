{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Common properties of judgements and relations.

module Koshucode.Baala.Type.Judge.JudgeClass
  ( GetClass (..),
    GetTerms (..),
    GetTermNames (..),
    getTermNamesUnique,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Syntax     as S

-- | Get judge class.
class GetClass a where
    getClass :: a -> S.JudgeClass

-- | Get term list.
class GetTerms a where
    getTerms :: a c -> [S.Term c]

-- | Get list of term names.
class GetTermNames a where
    getTermNames :: a -> [S.TermName]

instance GetTermNames [S.TermName] where
    getTermNames = id

instance GetTermNames String where
    getTermNames = S.stringTermNames

-- | Get unique list of term names.
getTermNamesUnique :: (GetTermNames a) => a -> [S.TermName]
getTermNamesUnique = B.unique . getTermNames
