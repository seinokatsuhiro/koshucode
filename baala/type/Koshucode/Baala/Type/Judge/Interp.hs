{-# OPTIONS_GHC -Wall #-}

-- | Type for data interpretation.

module Koshucode.Baala.Type.Judge.Interp
  ( Interp (..),
    InterpWord (..),
    interp,
  ) where

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Type.Judge.Judge         as T
import qualified Koshucode.Baala.Type.Judge.JudgeClass    as T

-- | Data interpretation.
data Interp = Interp
    { interpWords :: [InterpWord]  -- ^ Sentence which interpret data.
    , interpTerms :: [S.TermName]  -- ^ Terms in sentence.
    } deriving (Show, Eq, Ord)

-- | Component of data interpretation.
data InterpWord
    = InterpText String            -- ^ Constant part of interpretation.
    | InterpTerm S.TermName        -- ^ Variable part of interpretation.
    deriving (Show, Eq, Ord)

instance T.GetTermNames Interp where
    getTermNames = interpTerms

instance B.MixEncode Interp where
    mixEncode Interp { interpWords = xs } =
        S.bracketWith S.bracketInterp
             $ B.mixJoin B.mix1 $ map B.mixEncode xs

instance B.MixEncode InterpWord where
    mixEncode (InterpText w) = B.mixString w
    mixEncode (InterpTerm n) = T.termNameToMix n

-- | Create data interpretation.
interp :: [InterpWord] -> Interp
interp ws = intp where
    terms = B.unique $ B.mapMaybe getTermName ws
    intp  = Interp { interpWords = ws
                   , interpTerms = terms }

getTermName :: InterpWord -> Maybe S.TermName
getTermName (InterpTerm n) = Just n
getTermName _              = Nothing

