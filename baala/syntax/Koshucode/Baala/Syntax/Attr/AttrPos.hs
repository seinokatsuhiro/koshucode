{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Positional attributes.

module Koshucode.Baala.Syntax.Attr.AttrPos
  ( -- * Positional attribute
    AttrPos (..),
    attrPosNameList,

    -- * Attribute name
    AttrName (..),
    isAttrNameRelmap,
    attrNameText,
    attrNameTrunk,

    -- * Positional name
    AttrNamePos,
    AttrTree,
    AttrSortTree,
    sortAttrTree,
  ) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Token         as D
import qualified Koshucode.Baala.Syntax.Attr.Message  as Msg


-- ----------------------  Positional attribute

-- | Positional attribute.
data AttrPos a
    = AttrPos0            -- ^ No attributes
    | AttrPos1 a          -- ^ Single attribute
    | AttrPos2 a a        -- ^ Two attributes
    | AttrPos3 a a a      -- ^ Three attributes
    | AttrPos4 a a a a    -- ^ Four attributes
    | AttrPosE [a]        -- ^ Enumerated attributes
    | AttrPosV a          -- ^ Variable-length attributes
    | AttrPos1V a a       -- ^ Single-and-variable-length attributes
    | AttrPos1Q a a       -- ^ Single-and-optional attributes
      deriving (Show, Eq, Ord)

-- | Name list of positional attributes.
attrPosNameList :: AttrPos a -> [a]
attrPosNameList pos = case pos of
    AttrPos0           -> []
    AttrPos1  a        -> [a]
    AttrPos2  a b      -> [a,b]
    AttrPos3  a b c    -> [a,b,c]
    AttrPos4  a b c d  -> [a,b,c,d]
    AttrPosE  as       -> as
    AttrPosV  a        -> [a]
    AttrPos1V a b      -> [a,b]
    AttrPos1Q a b      -> [a,b]


-- ----------------------  Attribute name

-- | Attribute name of relmap.
data AttrName
    = AttrNormal       String  -- ^ Normal attribute
    | AttrRelmapNormal String  -- ^ Attribute for submap
    | AttrRelmapLocal  String  -- ^ Attribute for submap with local relation references
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Test attribute name is for subrelmap.
isAttrNameRelmap :: AttrName -> Bool
isAttrNameRelmap (AttrRelmapNormal _)  = True
isAttrNameRelmap (AttrRelmapLocal  _)  = True
isAttrNameRelmap _                     = False

-- | String part of attribute names.
attrNameText :: AttrName -> String
attrNameText (AttrNormal       n)  = n
attrNameText (AttrRelmapNormal n)  = n
attrNameText (AttrRelmapLocal  n)  = n

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrNormal "@trunk"


-- --------------------------------------------  Positional name

-- | Positional attribute
type AttrNamePos = AttrPos AttrName

-- | Positional attribute sorter.
type AttrSortTree = [D.TTree] -> B.Ab [AttrTree]

-- | Attribute name and its contents.
type AttrTree = (AttrName, [D.TTree])

-- | Sort token trees by positional attributes.
sortAttrTree :: AttrNamePos -> AttrSortTree
sortAttrTree (AttrPos0)         []         = Right []
sortAttrTree (AttrPos1 a)       [v]        = Right [a#v]
sortAttrTree (AttrPos2 a b)     [v,w]      = Right [a#v, b#w]
sortAttrTree (AttrPos3 a b c)   [v,w,x]    = Right [a#v, b#w, c#x]
sortAttrTree (AttrPos4 a b c d) [v,w,x,y]  = Right [a#v, b#w, c#x, d#y]
sortAttrTree (AttrPosE _)       xs         = Right $ zip enumAttr $ map B.li1 xs
sortAttrTree (AttrPosV a)       vv         = Right [a##vv]
sortAttrTree (AttrPos1V a b)    (v:ww)     = Right [a#v, b##ww]
sortAttrTree (AttrPos1Q a _)    [v]        = Right [a#v]
sortAttrTree (AttrPos1Q a b)    [v,w]      = Right [a#v, b#w]

sortAttrTree (AttrPos0)         _          = Msg.unexpAttr0
sortAttrTree (AttrPos1 _)       _          = Msg.unexpAttr1
sortAttrTree (AttrPos2 _ _)     _          = Msg.unexpAttr2
sortAttrTree (AttrPos3 _ _ _)   _          = Msg.unexpAttr3
sortAttrTree (AttrPos4 _ _ _ _) _          = Msg.unexpAttr4
sortAttrTree (AttrPos1V _ _)    _          = Msg.unexpAttr1V
sortAttrTree (AttrPos1Q _ _)    _          = Msg.unexpAttr1Q

(#) :: a -> v -> (a, [v])
a # v = (a ,[v])

(##) :: a -> vv -> (a, vv)
a ## vv = (a, vv)

enumAttr :: [AttrName]
enumAttr = map (AttrNormal . show) [1 :: Int ..]

-- isTermLeaf :: B.TTree -> Bool
-- isTermLeaf (B.TreeL token) = B.isTermToken token
-- isTermLeaf _               = False
