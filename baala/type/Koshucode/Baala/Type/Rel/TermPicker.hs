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
    pickDirect,
    pickTermsIndex,
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
termPicker :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> TermPicker c
termPicker left right = B.picker ls rs where
    (ls, rs) = getTermNamesUnique2 left right

-- | Double of something.
type Dbl a = (a, a)

-- | Get pair of term names.
getTermNamesUnique2 :: (D.GetTermNames a, D.GetTermNames b) => a -> b -> Dbl [S.TermName]
getTermNamesUnique2 l r = (D.getTermNamesUnique l, D.getTermNamesUnique r)


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

-- | Pick contents.
--
--   >>> pickDirect "/a /c" "/a /b /c" (words "1 2 3")
--   ["1", "3"]
--
pickDirect :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> O.Map [c]
pickDirect t1 t2 = pickTerms $ termPicker t1 t2

-- | Extract indices of terms.
--
--   >>> pickTermsIndex $ termPicker "/b /d" "/a /b /d /e"
--   [1,2]
--
pickTermsIndex :: TermPicker c -> [Int]
pickTermsIndex = B.pkRShareIndex

-- | Pick terms according to term picker.
pickTerms :: TermPicker c -> O.Map [c]
pickTerms = B.pkRShare

-- | Cut terms according to term picker.
cutTerms :: TermPicker c -> O.Map [c]
cutTerms = B.pkRProper

-- | Move terms forward.
forwardTerms :: TermPicker c -> O.Map [c]
forwardTerms = B.pkRForward

-- | Move terms backward.
backwardTerms :: TermPicker c -> O.Map [c]
backwardTerms = B.pkRBackward

-- | Move terms forward ('True') or backward ('False').
--
--   >>> let pk = termPicker "/c /b" "/a /b /c /d"
--   >>> towardTerms True pk (words "A B C D")
--   ["C","B","A","D"]
--   >>> towardTerms False pk (words "A B C D")
--   ["A","D","C","B"]
--
towardTerms :: Bool -> TermPicker c -> O.Map [c]
towardTerms True  = forwardTerms
towardTerms False = backwardTerms

