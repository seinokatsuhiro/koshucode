{-# OPTIONS_GHC -Wall #-}

-- | Shared and side terms.

module Koshucode.Baala.Data.Type.Rel.ShareSide
  ( ShareSide (..),
    ShareSideMap,
    ShareSideMap2,
    shareSide, shareSideOrd,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Data.Type.Judge     as D

type ShareSideMap c = ShareSide c -> [c] -> [c]
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

    , ssRForward     :: [c] -> [c]
    , ssRBackward    :: [c] -> [c]

    , ssRSplit       :: [c] -> ([c], [c])  -- ^ Pick right-shared and right-side part
    , ssRAssoc       :: [c] -> ([c], [c])  -- ^ Pick right-shared part and right contents
    }

-- | Create share-side structure from left and right term names.
shareSide :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> ShareSide c
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

shareSideOrd :: (D.GetTermNames l, D.GetTermNames r) => l -> r -> ShareSide c
shareSideOrd left right = shareSideBody li ri left' right' where
    (li, ri)  = (ind2 left', ind2 right')
    ind       = B.snipIndex left' right'
    ind2      = B.snipIndex $ B.snipFrom ind right'
    left'     = D.getTermNames left
    right'    = D.getTermNames right

shareSideBody :: [Int] -> [Int] -> [S.TermName] -> [S.TermName] -> ShareSide a
shareSideBody li ri left right = lr where
    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rfor       = B.snipForward  ri
    rback      = B.snipBackward ri
    rsplit xs  = (rshare xs, rside xs)
    rassoc xs  = (rshare xs, xs)

    lr = ShareSide
         { ssLShareIndex  = li
         , ssRShareIndex  = ri
         , ssDisjoint     = null li
         , ssLSideNames   = lside  left
         , ssLShareNames  = lshare left
         , ssRShareNames  = rshare right
         , ssRSideNames   = rside  right
         , ssLSide        = lside
         , ssLShare       = lshare
         , ssRShare       = rshare
         , ssRSide        = rside
         , ssRForward     = rfor
         , ssRBackward    = rback
         , ssRSplit       = rsplit
         , ssRAssoc       = rassoc
         }

