{-# OPTIONS_GHC -Wall #-}

-- | Shared and side terms.

module Koshucode.Baala.Data.Type.Rel.ShareSide
  ( -- * Picker
    HeadLR (..),
    HeadLRMap,
    HeadLRMap2,
    shareSide, shareSideOrd,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Data.Type.Judge     as D

type HeadLRMap c = HeadLR c -> [c] -> [c]
type HeadLRMap2 a b = (HeadLRMap a, HeadLRMap b)

-- | Shared and side terms.
data HeadLR c = HeadLR
    { headLShareIndex  :: [Int]         -- ^ Indicies of right-shared part
    , headRShareIndex  :: [Int]         -- ^ Indicies of left-shared part
    , headDisjoint     :: Bool          -- ^ Whether shared part is empty

    , headLSideNames   :: [S.TermName]  -- ^ Left-side term names
    , headLShareNames  :: [S.TermName]  -- ^ Left-shared term names
    , headRShareNames  :: [S.TermName]  -- ^ Right-shared term names
    , headRSideNames   :: [S.TermName]  -- ^ Right-side term names

    , headLSide        :: [c] -> [c]    -- ^ Pick left-side part from left contents
    , headLShare       :: [c] -> [c]    -- ^ Pick left-shared part from left contents
    , headRShare       :: [c] -> [c]    -- ^ Pick right-shared part from right contents
    , headRSide        :: [c] -> [c]    -- ^ Pick right-side part from right contents

    , headRForward     :: [c] -> [c]
    , headRBackward    :: [c] -> [c]

    , headRSplit       :: [c] -> ([c], [c])  -- ^ Pick right-shared and right-side part
    , headRAssoc       :: [c] -> ([c], [c])  -- ^ Pick right-shared part and right contents
    }

-- | Create share-side structure from left and right term names.
shareSide :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> HeadLR c
shareSide left right = shareSideBody li ri left' right' where
    (li, ri)  = sharedIndex left' right'
    left'     = D.getTermNames left
    right'    = D.getTermNames right

-- sharedIndex "dxcy" "abcd"
-- >>> ([0,2], [3,2])
--       d c    d c
sharedIndex :: (Ord a) => [a] -> [a] -> ([Int], [Int])
sharedIndex xs1 xs2 = (ind1, ind2) where
    ind1  = B.snipIndex sh xs1
    ind2  = B.snipIndex sh xs2
    sh    = B.intersectionFilter xs2 xs1

shareSideOrd :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> HeadLR c
shareSideOrd left right = shareSideBody li ri left' right' where
    (li, ri)  = (ind2 left', ind2 right')
    ind       = B.snipIndex left' right'
    ind2      = B.snipIndex $ B.snipFrom ind right'
    left'     = D.getTermNames left
    right'    = D.getTermNames right

shareSideBody :: [Int] -> [Int] -> [S.TermName] -> [S.TermName] -> HeadLR a
shareSideBody li ri left right = lr where
    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rfor       = B.snipForward  ri
    rback      = B.snipBackward ri
    rsplit xs  = (rshare xs, rside xs)
    rassoc xs  = (rshare xs, xs)

    lr = HeadLR { headLShareIndex  = li
                , headRShareIndex  = ri
                , headDisjoint     = null li
                , headLSideNames   = lside  left
                , headLShareNames  = lshare left
                , headRShareNames  = rshare right
                , headRSideNames   = rside  right
                , headLSide        = lside
                , headLShare       = lshare
                , headRShare       = rshare
                , headRSide        = rside
                , headRForward     = rfor
                , headRBackward    = rback
                , headRSplit       = rsplit
                , headRAssoc       = rassoc
                }
