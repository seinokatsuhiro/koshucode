{-# OPTIONS_GHC -Wall #-}

-- | Picker is a data for picking target elements
--   based on element names.

module Koshucode.Baala.Base.List.Picker
  ( picker,
    Picker (..),
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base.List.Select      as B
import qualified Koshucode.Baala.Base.List.Set         as B

-- | Create picker.
picker :: (Ord n) => [n] -> [n] -> Picker n c
picker ls rs = pk where
    -- index
    ls'        = B.unique ls
    rs'        = B.unique rs
    (li, ri)   = B.selectIndexBoth ls' rs'

    -- map
    properL    = B.selectOthers li
    shareL     = B.selectElems  li
    shareR     = B.selectElems  ri
    properR    = B.selectOthers ri

    -- split
    splitR xs  = (shareR xs, properR xs)
    assocR xs  = (shareR xs, xs)

    pk = Picker
         { pkDisjoint       = null li

         -- index
         , pkLShareIndex    = li
         , pkRShareIndex    = ri

         -- name
         , pkLNames         = ls
         , pkRNames         = rs
         , pkLProperNames   = properL ls'
         , pkLShareNames    = shareL  ls'
         , pkRShareNames    = shareR  rs'
         , pkRProperNames   = properR rs'

         -- map
         , pkLProper        = properL
         , pkLShare         = shareL
         , pkRShare         = shareR
         , pkRProper        = properR
         , pkRForward       = B.permuteForward  ri
         , pkRBackward      = B.permuteBackward ri

         -- split
         , pkRSplit         = splitR
         , pkRAssoc         = assocR
         }

-- | Picker data.
--
--   For example, __\/a \/b \/c__ and __\/b \/c \/d \/e__
--   have left-proper names __\/a__,
--   shared names __\/b \/c__,
--   and right-proper names __\/d \/e__.
--
--   >>> let pk = picker (words "/a /b /c") (words "/b /c /d /e")
--
data Picker n c = Picker
    { pkDisjoint :: Bool
      -- ^ __Test:__ Whether shared part is empty
      --
      --   >>> pkDisjoint pk
      --   False
      --
      --   >>> pkDisjoint $ picker (words "/a /b /c") (words "/d /e")
      --   True

    , pkLShareIndex :: [Int]
      -- ^ __Index:__ Indicies of right-shared part
      --
      --   >>> pkLShareIndex pk
      --   [1, 2]

    , pkRShareIndex :: [Int]
      -- ^ __Index:__ Indicies of left-shared part
      --
      --   >>> pkRShareIndex pk
      --   [0, 1]

    , pkLNames :: [n]
      -- ^ __Name:__ Left names
      --
      --   >>> pkLNames pk
      --   ["/a","/b","/c"]

    , pkRNames :: [n]
      -- ^ __Name:__ Right names
      --
      --   >>> pkRNames pk
      --   ["/b","/c","/d","/e"]

    , pkLProperNames :: [n]
      -- ^ __Name:__ Left-proper names
      --
      --   >>> pkLProperNames pk
      --   ["/a"]

    , pkLShareNames :: [n]
      -- ^ __Name:__ Left-shared names
      --
      --   >>> pkLShareNames pk
      --   ["/b","/c"]

    , pkRProperNames :: [n]
      -- ^ __Name:__ Right-proper names
      --
      --   >>> pkRProperNames pk
      --   ["/d","/e"]

    , pkRShareNames :: [n]
      -- ^ __Name:__ Right-shared names
      --
      --   >>> pkRShareNames pk
      --   ["/b","/c"]

    , pkLProper :: O.Map [c]
      -- ^ __Map:__ Pick left-proper part from left contents
      --
      --   >>> pkLProper pk "ABC"
      --   "A"

    , pkLShare :: O.Map [c]
      -- ^ __Map:__ Pick left-shared part from left contents
      --
      --   >>> pkLShare pk "ABC"
      --   "BC"

    , pkRShare :: O.Map [c]
      -- ^ __Map:__ Pick right-shared part from right contents
      --
      --   >>> pkRShare pk "BCDE"
      --   "BC"

    , pkRProper :: O.Map [c]
      -- ^ __Map:__ Pick right-proper part from right contents
      --
      --   >>> pkRProper pk "BCDE"
      --   "DE"

    , pkRForward :: O.Map [c]
      -- ^ __Map:__ Move shared names forward.
      --
      --   >>> pkRForward pk "BCDE"
      --   "BCDE"

    , pkRBackward :: O.Map [c]
      -- ^ __Map:__ Move shared names backward.
      --
      --   >>> pkRBackward pk "BCDE"
      --   "DEBC"

    , pkRSplit :: [c] -> ([c], [c])
      -- ^ __Split:__ Pick right-shared and right-proper part
      --
      --   >>> pkRSplit pk "BCDE"
      --   ("BC", "DE")

    , pkRAssoc :: [c] -> ([c], [c])
      -- ^ __Split:__ Pick right-shared part and right contents
      --
      --   >>> pkRAssoc pk "BCDE"
      --   ("BC", "BCDE")
    }

-- | @Picker (@ left-proper @:@ left-shared @|@ right-shared @:@ right-proper @)@
instance (Show n) => Show (Picker n c) where
    show Picker { pkLProperNames  = lp
                , pkLShareNames   = ls
                , pkRShareNames   = rs
                , pkRProperNames  = rp }
        = "Picker ( " ++ ws lp ++ " : " ++ ws ls ++
                " | " ++ ws rs ++ " : " ++ ws rp ++ " )"
          where ws = unwords . map show

