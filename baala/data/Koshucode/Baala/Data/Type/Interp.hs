{-# OPTIONS_GHC -Wall #-}

-- | Type for data interpretation.

module Koshucode.Baala.Data.Type.Interp
  ( Interp (..),
    InterpWord (..),
    interp,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data.Type.Judge   as D

-- | Data interpretation.
data Interp = Interp
    { interpWords :: [InterpWord]  -- ^ Sentence which interpret data.
    , interpTerms :: [S.TermName]  -- ^ Terms in sentence.
    } deriving (Show, Eq, Ord)

data InterpWord
    = InterpText String
    | InterpTerm S.TermName
    deriving (Show, Eq, Ord)

instance B.MixEncode Interp where
    mixEncode Interp { interpWords = xs } =
        B.mixBracketS S.interpOpen S.interpClose
             $ B.mixJoin B.mix1 $ map B.mixEncode xs

instance B.MixEncode InterpWord where
    mixEncode (InterpText w) = B.mixString w
    mixEncode (InterpTerm n) = D.termNameToMix n

interp :: [InterpWord] -> Interp
interp ws = intp where
    terms = B.unique $ B.mapMaybe getTermName ws
    intp  = Interp { interpWords = ws
                   , interpTerms = terms }

getTermName :: InterpWord -> Maybe S.TermName
getTermName (InterpTerm n) = Just n
getTermName _              = Nothing

