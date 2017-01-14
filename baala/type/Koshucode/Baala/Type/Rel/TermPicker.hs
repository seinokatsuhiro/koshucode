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

    -- * Double mapping
    pickTerms2, cutTerms2,
    forwardTerms2, backwardTerms2,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Judge            as T


-- ============================================  Construct

-- | Data for picking shared and proper terms.
type TermPicker c = B.Picker S.TermName c

-- | Type for picking terms.
type TermPick c = TermPicker c -> [c] -> [c]

-- | Double picker.
type TermPick2 a b = (TermPick a, TermPick b)

-- | Create term picker from left and right term names
termPicker :: (T.GetTermNames target, T.GetTermNames input)
           => target -> input -> TermPicker c
termPicker target input =
    B.picker (T.getTermNames target)
             (T.getTermNames input)


-- ============================================  Present & new terms

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
preTermsExist = B.some . preTerms

-- | Test new terms exist.
--
--   >>> newTermsExist $ termPicker "/a /b /c" "/a /b /d /e"
--   True
--
--   >>> newTermsExist $ termPicker "/a /b" "/a /b /d /e"
--   False
--
newTermsExist :: O.Test (TermPicker c)
newTermsExist = B.some . newTerms


--  ============================================  Mapping

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
pickDirect :: (T.GetTermNames target, T.GetTermNames input)
           => target -> input -> O.Map [c]
pickDirect target input = pickTerms $ termPicker target input

-- | Pick target terms from input terms.
--
--   >>> pickTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "BD"
--
pickTerms :: TermPick c
pickTerms = B.pkRShare

-- | Cut target terms from input terms.
--
--   >>> cutTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "AC"
--
cutTerms :: TermPick c
cutTerms = B.pkRProper

-- | Move target terms forward.
--
--   >>> forwardTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "BDAC"
--
forwardTerms :: TermPick c
forwardTerms = B.pkRForward

-- | Move target terms backward.
--
--   >>> backwardTerms (termPicker "/b /d" "/a /b /c /d") "ABCD"
--   "ACBD"
--
backwardTerms :: TermPick c
backwardTerms = B.pkRBackward

-- | Move target terms forward if 'True' or backward if 'False'.
towardTerms :: Bool -> TermPick c
towardTerms True  = forwardTerms
towardTerms False = backwardTerms


--  ============================================  Double mapping

-- | Double 'pickTerms'.
pickTerms2 :: TermPick2 a b
pickTerms2 = (pickTerms, pickTerms)

-- | Double 'cutTerms'.
cutTerms2 :: TermPick2 a b
cutTerms2 = (cutTerms, cutTerms)

-- | Double 'forwardTerms'.
forwardTerms2 :: TermPick2 a b
forwardTerms2 = (forwardTerms, forwardTerms)

-- | Double 'backwardTerms'.
backwardTerms2 :: TermPick2 a b
backwardTerms2 = (backwardTerms, backwardTerms)

