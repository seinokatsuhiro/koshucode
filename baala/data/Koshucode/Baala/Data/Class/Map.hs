{-# OPTIONS_GHC -Wall #-}

-- | Content map.

module Koshucode.Baala.Data.Class.Map
  ( -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,

    -- * Map
    contentAp, contentMap,
    contentApText,
    contentMapTextList,
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.Data.Class.Simple        as D
import qualified Koshucode.Baala.Data.Class.Complex       as D


-- ----------------------  Get & Put

-- | Pair of get and put.
type CGetPut a c = (c -> a, a -> c)

-- | 'gText' and 'pText'.
gpText :: (D.CText c) => CGetPut String c
gpText = (D.gText, D.pText)

-- | 'gList' and 'pList'.
gpList :: (D.CList c) => CGetPut [c] c
gpList = (D.gList, D.pList)

-- | 'gSet' and 'pSet'.
gpSet :: (D.CSet c) => CGetPut [c] c
gpSet = (D.gSet, D.pSet)

-- | 'gSetSort' and 'pSet'.
gpSetSort :: (Ord c, D.CSet c) => CGetPut [c] c
gpSetSort = (D.gSetSort, D.pSet)


-- ----------------------  Map

-- | Apply function to internal value of content.
--
--   >>> contentAp gText pText reverse (the $ pText "abc") :: BaalaC
--   VText "cba"
--
contentAp :: (c -> a) -> (b -> c') -> (a -> b) -> c -> c'
contentAp get put f = put . f . get

-- | Map function to internal value of content.
contentMap :: (Functor f) => (c -> f a) -> (f b -> c') -> (a -> b) -> c -> c'
contentMap get put f = contentAp get put $ fmap f

-- | Apply function to internal string of text content.
--
--   >>> contentApText reverse (the $ pText "abc")
--   VText "cba"
--
contentApText :: (D.CText c) => O.StringMap -> O.Map c
contentApText = contentAp D.gText D.pText

-- | Map function to characters of internal string of text content.
--
--   >>> contentMapTextList (\c -> pText [c]) (the $ pText "abc")
--   VList [VText "a",VText "b",VText "c"]
--
contentMapTextList :: (D.CList c, D.CText c) => (Char -> c) -> O.Map c
contentMapTextList = contentMap D.gText D.pList

