{-# OPTIONS_GHC -Wall #-}

-- | Term picker is a data for picking target terms
--   based on input heading terms.

module Koshucode.Baala.Type.Rel.TermPicker
  ( -- * Construct
    Picker (..),
    TermPicker,
    TermPick,
    TermPick2,
    termPicker,
    -- * Pre & new terms
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
--
--   For example, __\/a \/b \/c__ and __\/b \/c \/d \/e__
--   have left-proper terms __\/a__,
--   shared terms __\/b \/c__,
--   and right-proper terms __\/d \/e__.
--
--   >>> let pk = termPicker "/a /b /c" "/b /c /d /e"
--
data Picker n c = Picker
    { ssDisjoint :: Bool
      -- ^ __Test:__ Whether shared part is empty
      --
      --   >>> ssDisjoint pk
      --   False
      --
      --   >>> ssDisjoint $ termPicker "/a /b /c" "/d /e"
      --   True

    , ssLShareIndex :: [Int]
      -- ^ __Index:__ Indicies of right-shared part
      --
      --   >>> ssLShareIndex pk
      --   [1, 2]

    , ssRShareIndex :: [Int]
      -- ^ __Index:__ Indicies of left-shared part
      --
      --   >>> ssRShareIndex pk
      --   [0, 1]

    , ssLProperNames :: [n]
      -- ^ __Name:__ Left-proper term names
      --
      --   >>> ssLProperNames pk
      --   [TermName EQ "a"]

    , ssLShareNames :: [n]
      -- ^ __Name:__ Left-shared term names
      --
      --   >>> ssLShareNames pk
      --   [TermName EQ "b", TermName EQ "c"]

    , ssRProperNames :: [n]
      -- ^ __Name:__ Right-proper term names
      --
      --   >>> ssRProperNames pk
      --   [TermName EQ "d", TermName EQ "e"]

    , ssRShareNames :: [n]
      -- ^ __Name:__ Right-shared term names
      --
      --   >>> ssRShareNames pk
      --   [TermName EQ "b", TermName EQ "c"]

    , ssLProper :: O.Map [c]
      -- ^ __Map:__ Pick left-proper part from left contents
      --
      --   >>> ssLProper pk "ABC"
      --   "A"

    , ssLShare :: O.Map [c]
      -- ^ __Map:__ Pick left-shared part from left contents
      --
      --   >>> ssLShare pk "ABC"
      --   "BC"

    , ssRShare :: O.Map [c]
      -- ^ __Map:__ Pick right-shared part from right contents
      --
      --   >>> ssRShare pk "BCDE"
      --   "BC"

    , ssRProper :: O.Map [c]
      -- ^ __Map:__ Pick right-proper part from right contents
      --
      --   >>> ssRProper pk "BCDE"
      --   "DE"

    , ssRForward :: O.Map [c]
      -- ^ __Map:__ Move shared terms forward.
      --
      --   >>> ssRForward pk "BCDE"
      --   "BCDE"

    , ssRBackward :: O.Map [c]
      -- ^ __Map:__ Move shared terms backward.
      --
      --   >>> ssRBackward pk "BCDE"
      --   "DEBC"

    , ssRSplit :: [c] -> ([c], [c])
      -- ^ __Split:__ Pick right-shared and right-proper part
      --
      --   >>> ssRSplit pk "BCDE"
      --   ("BC", "DE")

    , ssRAssoc :: [c] -> ([c], [c])
      -- ^ __Split:__ Pick right-shared part and right contents
      --
      --   >>> ssRAssoc pk "BCDE"
      --   ("BC", "BCDE")
    }

-- | Create picker.
picker :: (Ord n) => [n] -> [n] -> Picker n c
picker ls rs = pk where
    -- index
    (li, ri)   = B.selectIndexBoth ls rs

    -- map
    properL    = B.selectOthers    li
    shareL     = B.selectElems     li
    shareR     = B.selectElems     ri
    properR    = B.selectOthers    ri

    -- split
    splitR xs  = (shareR xs, properR xs)
    assocR xs  = (shareR xs, xs)

    pk = Picker
         { ssDisjoint       = null li

         -- index
         , ssLShareIndex    = li
         , ssRShareIndex    = ri

         -- name
         , ssLProperNames   = properL ls
         , ssLShareNames    = shareL  ls
         , ssRShareNames    = shareR  rs
         , ssRProperNames   = properR rs

         -- map
         , ssLProper        = properL
         , ssLShare         = shareL
         , ssRShare         = shareR
         , ssRProper        = properR
         , ssRForward       = B.permuteForward  ri
         , ssRBackward      = B.permuteBackward ri

         -- split
         , ssRSplit         = splitR
         , ssRAssoc         = assocR
         }

-- | Data for picking shared and proper terms.
type TermPicker c = Picker S.TermName c

-- | Type for picking terms.
type TermPick c = TermPicker c -> [c] -> [c]

-- | Double picker.
type TermPick2 a b = (TermPick a, TermPick b)

-- | Create term picker from left and right term names.
termPicker :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> TermPicker c
termPicker left right = picker ls rs where
    (ls, rs) = getTermNamesUnique2 left right

-- | Double of something.
type Dbl a = (a, a)

-- | Get pair of term names.
getTermNamesUnique2 :: (D.GetTermNames a, D.GetTermNames b) => a -> b -> Dbl [S.TermName]
getTermNamesUnique2 l r = (D.getTermNamesUnique l, D.getTermNamesUnique r)


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
newTerms = ssLProperNames

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
--   >>> pickDirect "/a /c" "/a /b /c" (words "1 2 3")
--   ["1", "3"]
--
pickDirect :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> O.Map [c]
pickDirect t1 t2 = pickTerms $ termPicker t1 t2

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
cutTerms = ssRProper

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

