{-# OPTIONS_GHC -Wall #-}

-- | Queue.

module Koshucode.Baala.Core.Resource.Queue
  ( -- * Data type
    Queue,

    -- * Construction
    qFrom, qTo,
    qNull, qLength,

    -- * Enqueue and dequeue
    enq, enqs, deq,
  ) where

import qualified Koshucode.Baala.Base   as B


-- --------------------------------------------  Data type

-- | First-in first-out list.
data Queue a = Queue [a] [a]
    deriving (Eq, Ord)

instance (Show a) => Show (Queue a) where
    show q = "Queue " ++ show (qTo q)

-- | Empty queue
instance B.Default (Queue a) where
    def = Queue [] []


-- --------------------------------------------  Construction

-- | Create queue from list.
qFrom :: [a] -> Queue a
qFrom = Queue []

-- | Convert queue to list.
qTo :: Queue a -> [a]
qTo (Queue qi qo) = qo ++ reverse qi

-- | Test queue is empty.
qNull :: Queue a -> Bool
qNull (Queue [] [])  = True
qNull _              = False

-- | Number of elements in queue.
qLength :: Queue a -> Int
qLength (Queue qi qo) = length qi + length qo


-- --------------------------------------------  Enqueue and dequeue

-- | Enqueue element.
--
--   >>> enq 2 $ enq 1 (B.def :: Queue Int)
--   Queue [1,2]
enq :: a -> Queue a -> Queue a
enq a (Queue qi qo) = Queue (a : qi) qo

-- | Enqueue multiple elements.
--
--   >>> enqs [1,2,3] (B.def :: Queue Int)
--   Queue [1,2,3]
enqs :: [a] -> Queue a -> Queue a
enqs ls (Queue qi qo) = Queue (reverse ls ++ qi) qo

-- | Dequeue.
--
--   >>> deq $ enq 2 $ enq 1 (B.def :: Queue Int)
--   (Just 1, Queue [2])
--
--   >>> deq (B.def :: Queue Int)
--   (Nothing, Queue [])
deq :: Queue a -> (Maybe a, Queue a)
deq (Queue qi (a : qo))  = (Just a,  Queue qi qo)
deq (Queue [] [])        = (Nothing, Queue [] [])
deq (Queue qi [])        = deq (Queue [] $ reverse qi)

