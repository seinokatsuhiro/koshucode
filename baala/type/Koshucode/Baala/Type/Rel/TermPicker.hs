{-# OPTIONS_GHC -Wall #-}

-- | Term picker is a data for picking target terms
--   based on input heading terms.

module Koshucode.Baala.Type.Rel.TermPicker
  ( -- * Construct
    TermPicker (..),
    TermPick,
    TermPick2,
    termPicker,
    -- * Pre & new terms
    preTerms, newTerms, 
    preTermsExist, newTermsExist,
    -- * Mapping
    picker,
    pickTermsIndex,
    pickTerms, cutTerms,
    forwardTerms, backwardTerms, towardTerms,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Judge            as D


-- ---------------------- * Construct

-- | Type for picking terms.
type TermPick c = TermPicker c -> [c] -> [c]

-- | Double picker.
type TermPick2 a b = (TermPick a, TermPick b)

-- | Data for picking shared and proper terms.
--
--   For example, __\/a \/b \/c__ and __\/b \/c \/d \/e__
--   have left-proper terms __\/a__,
--   shared terms __\/b \/c__,
--   and right-proper terms __\/d \/e__.
--
--   >>> let pk = termPicker "/a /b /c" "/b /c /d /e"
--
data TermPicker c = TermPicker
    { ssLShareIndex  :: [Int]
      -- ^ Indicies of right-shared part
      --
      --   >>> ssLShareIndex pk
      --   [1, 2]

    , ssRShareIndex  :: [Int]
      -- ^ Indicies of left-shared part
      --
      --   >>> ssRShareIndex pk
      --   [0, 1]

    , ssDisjoint     :: Bool
      -- ^ Whether shared part is empty
      --
      --   >>> ssDisjoint pk
      --   False
      --
      --   >>> ssDisjoint $ termPicker "/a /b /c" "/d /e"
      --   True

    , ssLSideNames   :: [S.TermName]
      -- ^ Left-proper term names
      --
      --   >>> ssLSideNames pk
      --   [TermName EQ "a"]

    , ssLShareNames  :: [S.TermName]
      -- ^ Left-shared term names
      --
      --   >>> ssLShareNames pk
      --   [TermName EQ "b", TermName EQ "c"]

    , ssRShareNames  :: [S.TermName]
      -- ^ Right-shared term names
      --
      --   >>> ssRShareNames pk
      --   [TermName EQ "b", TermName EQ "c"]

    , ssRSideNames   :: [S.TermName]
      -- ^ Right-proper term names
      --
      --   >>> ssRSideNames pk
      --   [TermName EQ "d", TermName EQ "e"]

    , ssLSide        :: [c] -> [c]
      -- ^ Pick left-proper part from left contents
      --
      --   >>> ssLSide pk "ABC"
      --   "A"

    , ssLShare       :: [c] -> [c]
      -- ^ Pick left-shared part from left contents
      --
      --   >>> ssLShare pk "ABC"
      --   "BC"

    , ssRShare       :: [c] -> [c]
      -- ^ Pick right-shared part from right contents
      --
      --   >>> ssRShare pk "BCDE"
      --   "BC"

    , ssRSide        :: [c] -> [c]
      -- ^ Pick right-proper part from right contents
      --
      --   >>> ssRSide pk "BCDE"
      --   "DE"

    , ssRSplit       :: [c] -> ([c], [c])
      -- ^ Pick right-shared and right-proper part
      --
      --   >>> ssRSplit pk "BCDE"
      --   ("BC", "DE")

    , ssRAssoc       :: [c] -> ([c], [c])
      -- ^ Pick right-shared part and right contents
      --
      --   >>> ssRAssoc pk "BCDE"
      --   ("BC", "BCDE")

    , ssRForward     :: [c] -> [c]
      -- ^ Move shared terms forward.
      --
      --   >>> ssRForward pk "BCDE"
      --   "BCDE"

    , ssRBackward    :: [c] -> [c]
      -- ^ Move shared terms backward.
      --
      --   >>> ssRBackward pk "BCDE"
      --   "DEBC"
    }

-- | Create term picker from left and right term names.
termPicker :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> TermPicker c
termPicker left right = termPickerBody (li, ri) (ln, rn) where
    (ln, rn) = getTermNamesUnique2 left right
    (li, ri) = B.selectIndexBoth ln rn

-- | Double of something.
type Dbl a = (a, a)

-- | Get pair of term names.
getTermNamesUnique2 :: (D.GetTermNames a, D.GetTermNames b) => a -> b -> Dbl [S.TermName]
getTermNamesUnique2 l r = (D.getTermNamesUnique l, D.getTermNamesUnique r)

termPickerBody :: Dbl [Int] -> Dbl [S.TermName] -> TermPicker a
termPickerBody (li, ri) (ln, rn) = ss where
    lside      = B.selectOthers    li
    lshare     = B.selectElems     li
    rshare     = B.selectElems     ri
    rside      = B.selectOthers    ri
    rfor       = B.permuteForward  ri
    rback      = B.permuteBackward ri
    rsplit xs  = (rshare xs, rside xs)
    rassoc xs  = (rshare xs, xs)

    ss = TermPicker
         { ssLShareIndex  = li
         , ssRShareIndex  = ri
         , ssDisjoint     = null li
         , ssLSideNames   = lside  ln
         , ssLShareNames  = lshare ln
         , ssRShareNames  = rshare rn
         , ssRSideNames   = rside  rn
         , ssLSide        = lside
         , ssLShare       = lshare
         , ssRShare       = rshare
         , ssRSide        = rside
         , ssRSplit       = rsplit
         , ssRAssoc       = rassoc
         , ssRForward     = rfor
         , ssRBackward    = rback
         }


-- ---------------------- * Pre & new terms

-- | List of present terms.
--
--   >>> preTerms $ termPicker (words "a b c") (words "a b d e")
--   ["a","b"]
--
preTerms :: TermPicker c -> [S.TermName]
preTerms = ssLShareNames

-- | List of new terms.
--
--   >>> newTerms $ termPicker (words "a b c") (words "a b d e")
--   ["c"]
--
newTerms :: TermPicker c -> [S.TermName]
newTerms = ssLSideNames

-- | Test present terms exist.
--
--   >>> preTermsExist $ termPicker (words "d e") (words "a b c")
--   False
--
--   >>> preTermsExist $ termPicker (words "c d e") (words "a b c")
--   True
--
preTermsExist :: O.Test (TermPicker c)
preTermsExist = B.notNull . preTerms

-- | Test new terms exist.
--
--   >>> newTermsExist $ termPicker (words "a b c") (words "a b d e")
--   True
--
--   >>> newTermsExist $ termPicker (words "a b") (words "a b d e")
--   False
--
newTermsExist :: O.Test (TermPicker c)
newTermsExist = B.notNull . newTerms


--  ---------------------- * Mapping

-- | Pick contents.
--
--   >>> picker (words "a c") (words "a b c") (words "1 2 3")
--   ["1", "3"]
--
picker :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> O.Map [c]
picker t1 t2 = pickTerms $ termPicker t1 t2

-- | Extract indices of terms.
--
--   >>> pickTermsIndex $ termPicker (words "b d") (words "a b d e")
--   [1,2]
--
pickTermsIndex :: TermPicker c -> [Int]
pickTermsIndex = ssRShareIndex

-- | Pick terms according to term picker.
pickTerms :: TermPicker c -> O.Map [c]
pickTerms = ssRShare

-- | Cut terms according to term picker.
cutTerms :: TermPicker c -> O.Map [c]
cutTerms = ssRSide

-- | Move terms forward.
forwardTerms :: TermPicker c -> O.Map [c]
forwardTerms = ssRForward

-- | Move terms backward.
backwardTerms :: TermPicker c -> O.Map [c]
backwardTerms = ssRBackward

-- | Move terms forward ('True') or backward ('False').
--
--   >>> let pk = termPicker (words "c b") (words "a b c d")
--   >>> towardTerms True pk (words "A B C D")
--   ["C","B","A","D"]
--   >>> towardTerms False pk (words "A B C D")
--   ["A","D","C","B"]
--
towardTerms :: Bool -> TermPicker c -> O.Map [c]
towardTerms True  = forwardTerms
towardTerms False = backwardTerms

