{-# OPTIONS_GHC -Wall #-}

-- | Shared and side terms.

module Koshucode.Baala.Data.Type.Rel.ShareSide
  ( ShareSide (..),
    ShareSideMap,
    ShareSideMap2,
    shareSide,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Data.Type.Judge     as D

-- | Share-side picker.
type ShareSideMap c = ShareSide c -> [c] -> [c]

-- | Double share-side picker.
type ShareSideMap2 a b = (ShareSideMap a, ShareSideMap b)

-- | Shared and side terms.
data ShareSide c = ShareSide
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

-- | Create share-side structure from left and right term names.
--
--     >>> let ss = shareSide ["a", "b", "c"] ["b", "c", "d", "e"]
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
shareSide :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> ShareSide c
shareSide left right = shareSideBody (li, ri) (ln, rn) where
    (ln, rn) = getTermNames2 left right
    (li, ri) = doubleIndex ln rn $ B.memberFilter rn ln

doubleIndex :: (Ord a) => [a] -> [a] -> [a] -> Dbl [Int]
doubleIndex ln rn xn = (B.snipIndex xn ln, B.snipIndex xn rn)

-- | Double of something.
type Dbl a = (a, a)

-- | Get pair of term names.
getTermNames2 :: (D.GetTermNames a, D.GetTermNames b) => a -> b -> Dbl [S.TermName]
getTermNames2 l r = (D.getTermNames l, D.getTermNames r)

shareSideBody :: Dbl [Int] -> Dbl [S.TermName] -> ShareSide a
shareSideBody (li, ri) (ln, rn) = ss where
    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rfor       = B.snipForward  ri
    rback      = B.snipBackward ri
    rsplit xs  = (rshare xs, rside xs)
    rassoc xs  = (rshare xs, xs)

    ss = ShareSide
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
