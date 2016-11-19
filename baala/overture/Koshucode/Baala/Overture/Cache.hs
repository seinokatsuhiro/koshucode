{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Cache.

module Koshucode.Baala.Overture.Cache
 ( -- * Example
   -- $Example

   -- * Library
   Cache,
   CacheS,
   cache,
   cacheGet,
   cacheGetList,
 ) where

import qualified Data.Map.Strict  as M

-- $Example
--
-- Create cache data with limit upto 3 elements
-- and identity value function.
--
--   > let cc0 = cache [3] id :: Cache String String
--
-- Get cached values.
--
--   > let (cc1, v1) = cacheGet cc0 "a"
--   > let (cc2, v2) = cacheGet cc1 "b"
--   > let (cc3, v3) = cacheGet cc2 "a"
--   > let (cc4, v4) = cacheGet cc3 "c"
--   > let (cc5, v5) = cacheGet cc4 "d"
--   > let (cc6, v6) = cacheGet cc5 "b"
--
-- Show the values.
--
--   >>> [v1,v2,v3,v4,v5,v6]
--   ["a","b","a","c","d","b"]
--
-- Show the caches.
--
--   >>> cc0
--   Cache 3 [][]
--   >>> cc1
--   Cache 3 ["a"][]
--   >>> cc2
--   Cache 3 ["a","b"][]
--   >>> cc3
--   Cache 3 ["a","b"][]
--   >>> cc4
--   Cache 3 ["a","b","c"][]
--   >>> cc5
--   Cache 3 ["d"]["a","b","c"]
--   >>> cc6
--   Cache 3 ["b","d"]["a","b","c"]

-- | Cache data.
data Cache k v = Cache
    { cacheVal       :: k -> v     -- ^ Value function
    , cacheLimit     :: Int        -- ^ Current size limit
    , cacheLimits    :: [Int]      -- ^ Extended limits
    , cacheA         :: M.Map k v  -- ^ Current cache map
    , cacheB         :: M.Map k v  -- ^ Previous cache map
    }

instance (Show k) => Show (Cache k v) where
    show Cache {..} = "Cache " ++ show cacheLimit ++ " "
                      ++ show (M.keys cacheA) ++ " "
                      ++ show (M.keys cacheB)

-- | String cache.
type CacheS v = Cache String v

-- | Create empty cache of given limits and value function.
cache :: [Int] -> (k -> v) -> Cache k v
cache (l:ls) = cacheWith l ls
cache []     = cache (cube <$> [2 .. 16])

cube :: (Num n) => n -> n
cube n = n * n * n

cacheWith :: Int -> [Int] -> (k -> v) -> Cache k v
cacheWith l ls val =
    Cache { cacheVal     = val
          , cacheLimit   = l
          , cacheLimits  = ls
          , cacheA       = M.empty
          , cacheB       = M.empty }

-- | Get cached value.
cacheGet :: (Ord k) => Cache k v -> k -> (Cache k v, v)
cacheGet cc@Cache {..} k =
    case M.lookup k cacheA of
      Just v -> (cc, v)
      Nothing -> case M.lookup k cacheB of
                   Just v  -> put v
                   Nothing -> put $ cacheVal k
    where
      put v | M.size cacheA < cacheLimit =
                let cc' = cc { cacheA = M.insert k v cacheA }
                in (cc', v)
            | otherwise = 
                let cc' = cc { cacheA = M.singleton k v
                             , cacheB = cacheA }
                in (nextLimit cc', v)

-- | Update size limit.
nextLimit :: Cache k v -> Cache k v
nextLimit cc@Cache{ cacheLimits = [] } = cc
nextLimit cc@Cache{ cacheLimits = l : ls } =
    cc { cacheLimit = l, cacheLimits = ls }

-- | Get cached values.
--
--   >>> cacheGetList (cache [] id :: Cache Char Char) ['A' .. 'Z']
--   (Cache 27 "ABCDEFGHIJKLMNOPQR" "STUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
--
cacheGetList :: (Ord k) => Cache k v -> [k] -> (Cache k v, [v])
cacheGetList cc0 = foldr get (cc0, []) where
    get k (cc, vs) = case cacheGet cc k of
                       (cc', v) -> (cc', v : vs)

