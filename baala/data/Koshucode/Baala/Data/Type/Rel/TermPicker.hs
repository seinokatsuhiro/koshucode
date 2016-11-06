{-# OPTIONS_GHC -Wall #-}

-- | Term picker.

module Koshucode.Baala.Data.Type.Rel.TermPicker
  ( TermPicker (..),
    TermPick,
    TermPick2,
    termPicker,
    preTerms, newTerms, 
    preTermsExist, newTermsExist,
    towardTerms,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Type.JudgeClass  as D

-- | Type for picking terms.
type TermPick c = TermPicker c -> [c] -> [c]

-- | Double picker.
type TermPick2 a b = (TermPick a, TermPick b)

-- | Data for picking terms.
data TermPicker c = TermPicker
    { ssLShareIndex  :: [Int]         -- ^ Indicies of right-shared part
    , ssRShareIndex  :: [Int]         -- ^ Indicies of left-shared part
    , ssDisjoint     :: Bool          -- ^ Whether shared part is empty

    , ssLSideNames   :: [S.TermName]  -- ^ Left-side term names
    , ssLShareNames  :: [S.TermName]  -- ^ Left-shared term names
    , ssRShareNames  :: [S.TermName]  -- ^ Right-shared term names
    , ssRSideNames   :: [S.TermName]  -- ^ Right-side term names

    , ssLSide        :: [c] -> [c]    -- ^ Pick left-side part from left contents
    , ssLShare       :: [c] -> [c]    -- ^ Pick left-shared part from left contents
    , ssRShare       :: [c] -> [c]    -- ^ Pick right-shared part from right contents
    , ssRSide        :: [c] -> [c]    -- ^ Pick right-side part from right contents

    , ssRSplit       :: [c] -> ([c], [c])  -- ^ Pick right-shared and right-side part
    , ssRAssoc       :: [c] -> ([c], [c])  -- ^ Pick right-shared part and right contents

    , ssRForward     :: [c] -> [c]    -- ^ Move shared terms forward.
    , ssRBackward    :: [c] -> [c]    -- ^ Move shared terms backward.
    }

-- | Create term picker from left and right term names.
--
--     >>> let ss = termPicker ["a", "b", "c"] ["b", "c", "d", "e"]
--
--     >>> ssDisjoint ss
--     False
--
--   Left terms.
--
--     >>> ssLShareIndex ss
--     [1, 2]
--
--     >>> ssLShareNames ss
--     ["b", "c"]
--
--     >>> ssLSide ss "ABC"
--     "A"
--
--     >>> ssLShare ss "ABC"
--     "BC"
--
--   Right terms.
--
--     >>> ssRShareIndex ss
--     [0, 1]
--
--     >>> ssRShareNames ss
--     ["b", "c"]
--
--     >>> ssRSideNames ss
--     ["d", "e"]
--
--     >>> ssRShare ss "BCDE"
--     "BC"
--
--     >>> ssRSide ss "BCDE"
--     "DE"
--
--     >>> ssRSplit ss "BCDE"
--     ("BC", "DE")
--
--     >>> ssRAssoc ss "BCDE"
--     ("BC", "BCDE")
--
--     >>> ssRForward ss "BCDE"
--     "BCDE"
--
--     >>> ssRBackward ss "BCDE"
--     "DEBC"
--
termPicker :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> TermPicker c
termPicker left right = termPickerBody (li, ri) (ln, rn) where
    (ln, rn) = getTermNames2 left right
    (li, ri) = doubleIndex ln rn $ B.memberFilter rn ln

doubleIndex :: (Ord a) => [a] -> [a] -> [a] -> Dbl [Int]
doubleIndex ln rn xn = (B.snipIndex xn ln, B.snipIndex xn rn)

-- | Double of something.
type Dbl a = (a, a)

-- | Get pair of term names.
getTermNames2 :: (D.GetTermNames a, D.GetTermNames b) => a -> b -> Dbl [S.TermName]
getTermNames2 l r = (D.getTermNames l, D.getTermNames r)

termPickerBody :: Dbl [Int] -> Dbl [S.TermName] -> TermPicker a
termPickerBody (li, ri) (ln, rn) = ss where
    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rfor       = B.snipForward  ri
    rback      = B.snipBackward ri
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

-- | Move terms forward ('True') or backward ('False').
--
--   >>> let pk = termPicker (words "c b") (words "a b c d")
--   >>> towardTerms True pk (words "A B C D")
--   ["C","B","A","D"]
--   >>> towardTerms False pk (words "A B C D")
--   ["A","D","C","B"]
--
towardTerms :: Bool -> TermPicker c -> O.Map [c]
towardTerms True  = ssRForward
towardTerms False = ssRBackward

