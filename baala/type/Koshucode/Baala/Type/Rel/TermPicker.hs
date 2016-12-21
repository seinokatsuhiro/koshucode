{-# OPTIONS_GHC -Wall #-}

-- | Term picker is a data for picking target terms
--   based on input heading terms.

module Koshucode.Baala.Type.Rel.TermPicker
  ( -- * Construct
    TermPicker,
    TermPick,
    TermPick2,
    termPicker,

    -- * Presend & new terms
    preTerms, newTerms, 
    preTermsExist, newTermsExist,

    -- * Mapping
    termsIndex,
    pickDirect,
    pickTerms, cutTerms,
    forwardTerms, backwardTerms, towardTerms,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Judge            as D


-- ---------------------- * Construct

-- | Data for picking shared and proper terms.
type TermPicker c = B.Picker S.TermName c

-- | Type for picking terms.
type TermPick c = TermPicker c -> [c] -> [c]

-- | Double picker.
type TermPick2 a b = (TermPick a, TermPick b)

-- | Create term picker from left and right term names
termPicker :: (D.GetTermNames target, D.GetTermNames input)
           => target -> input -> TermPicker c
termPicker target input =
    B.picker (D.getTermNamesUnique target)
             (D.getTermNamesUnique input)


-- ---------------------- * Present & new terms

-- | List of present terms.
--   The first argument of 'termPicker' is treated as target terms,
--   and the second is terms of input heading.
--   Because firstly target terms are given to relmap operator,
--   secondly input heading is fixed.
--
--   >>> preTerms $ termPicker "/a /b /c" "/a /b /d /e"
--   [TermName EQ "a",TermName EQ "b"]
--
preTerms :: TermPicker c -> [S.TermName]
preTerms = B.pkLShareNames

-- | List of new terms.
--
--   >>> newTerms $ termPicker "/a /b /c" "/a /b /d /e"
--   [TermName EQ "c"]
--
newTerms :: TermPicker c -> [S.TermName]
newTerms = B.pkLProperNames

-- | Test present terms exist.
--
--   >>> preTermsExist $ termPicker "/d /e" "/a /b /c"
--   False
--
--   >>> preTermsExist $ termPicker "/c /d /e" "/a /b /c"
--   True
--
preTermsExist :: O.Test (TermPicker c)
preTermsExist = B.notNull . preTerms

-- | Test new terms exist.
--
--   >>> newTermsExist $ termPicker "/a /b /c" "/a /b /d /e"
--   True
--
--   >>> newTermsExist $ termPicker "/a /b" "/a /b /d /e"
--   False
--
newTermsExist :: O.Test (TermPicker c)
newTermsExist = B.notNull . newTerms


--  ---------------------- * Mapping

-- | Indices of target terms on input terms.
--
--   >>> termsIndex $ termPicker "/b /d" "/a /b /c /d"
--   [1,3]
--
termsIndex :: TermPicker c -> [Int]
termsIndex = B.pkRShareIndex

-- | Pick contents of target terms.
--
--   >>> pickDirect "/b /d" "/a /b /c /d" "ABCD"
--   "BD"
--
pickDirect :: (D.GetTermNames target, D.GetTermNames input)
           => target -> input -> O.Map [c]
pickDirect target input = pickTerms $ termPicker target input

-- | Pick target terms from input terms.
--
--   >>> pickTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "BD"
--
pickTerms :: TermPicker c -> O.Map [c]
pickTerms = B.pkRShare

-- | Cut target terms from input terms.
--
--   >>> cutTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "AC"
--
cutTerms :: TermPicker c -> O.Map [c]
cutTerms = B.pkRProper

-- | Move target terms forward.
--
--   >>> forwardTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "BDAC"
--
forwardTerms :: TermPicker c -> O.Map [c]
forwardTerms = B.pkRForward

-- | Move target terms backward.
--
--   >>> backwardTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "ACBD"
--
backwardTerms :: TermPicker c -> O.Map [c]
backwardTerms = B.pkRBackward

-- | Move target terms forward if 'True' or backward if 'False'.
towardTerms :: Bool -> TermPicker c -> O.Map [c]
towardTerms True  = forwardTerms
towardTerms False = backwardTerms

