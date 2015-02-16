{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Positional attributes.

module Koshucode.Baala.Core.Lexmap.AttrPos
  ( -- * Positional attribute
    AttrPos (..),
    attrTypeNames,
    ropAttrPos,
    AttrSortTree,
    AttrTree,

    -- * Attribute name
    AttrName (..),
    isAttrNameRelmap, isAttrNameLocal,
    attrNameText,
    attrNameTrunk,
  ) where

import qualified Data.Generics                  as G
import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Core.Message   as Msg


-- ----------------------  Positional attribute

data AttrPos a
    = AttrPos0
    | AttrPos1 a
    | AttrPos2 a a
    | AttrPos3 a a a
    | AttrPos4 a a a a
    | AttrPosE [a]
    | AttrPosV a
    | AttrPos1V a a
    | AttrPos1Q a a
    | AttrPosT1 a a
    | AttrPosT2 a a a
      deriving (Show, Eq, Ord)

attrTypeNames :: AttrPos a -> [a]
attrTypeNames attrType = case attrType of
    AttrPos0           -> []
    AttrPos1  a        -> [a]
    AttrPos2  a b      -> [a,b]
    AttrPos3  a b c    -> [a,b,c]
    AttrPos4  a b c d  -> [a,b,c,d]
    AttrPosE  as       -> as
    AttrPosV  a        -> [a]
    AttrPos1V a b      -> [a,b]
    AttrPos1Q a b      -> [a,b]
    AttrPosT1 a b      -> [a,b]
    AttrPosT2 a b c    -> [a,b,c]

type AttrSortTree = [B.TTree] -> B.Ab [AttrTree]

-- | Attribute name and its contents.
type AttrTree = (AttrName, [B.TTree])

ropAttrPos :: AttrPos AttrName -> AttrSortTree
ropAttrPos (AttrPos0)         []         = Right []

ropAttrPos (AttrPos1 a)       [v]        = Right [a#v]
ropAttrPos (AttrPos2 a b)     [v,w]      = Right [a#v, b#w]
ropAttrPos (AttrPos3 a b c)   [v,w,x]    = Right [a#v, b#w, c#x]
ropAttrPos (AttrPos4 a b c d) [v,w,x,y]  = Right [a#v, b#w, c#x, d#y]
ropAttrPos (AttrPosE _)       xs         = Right $ zip enumAttr $ map B.li1 xs
ropAttrPos (AttrPosV a)       vv         = Right [a##vv]
ropAttrPos (AttrPos1V a b)    (v:ww)     = Right [a#v, b##ww]
ropAttrPos (AttrPos1Q a _)    [v]        = Right [a#v]
ropAttrPos (AttrPos1Q a b)    [v,w]      = Right [a#v, b#w]
ropAttrPos (AttrPosT1 a b)    xs         = case span isTermLeaf xs of
                                              (vv,[w])    -> Right [a##vv, b#w]
                                              _           -> Msg.unexpAttrT1
ropAttrPos (AttrPosT2 a b c)  xs         = case span isTermLeaf xs of
                                              (vv,[w,x])  -> Right [a##vv, b#w, c#x]
                                              _           -> Msg.unexpAttrT2
ropAttrPos (AttrPos0)         _          = Msg.unexpAttr0
ropAttrPos (AttrPos1 _)       _          = Msg.unexpAttr1
ropAttrPos (AttrPos2 _ _)     _          = Msg.unexpAttr2
ropAttrPos (AttrPos3 _ _ _)   _          = Msg.unexpAttr3
ropAttrPos (AttrPos4 _ _ _ _) _          = Msg.unexpAttr4
ropAttrPos (AttrPos1V _ _)    _          = Msg.unexpAttr1V
ropAttrPos (AttrPos1Q _ _)    _          = Msg.unexpAttr1Q

(#) :: a -> v -> (a, [v])
a # v = (a ,[v])

(##) :: a -> vv -> (a, vv)
a ## vv = (a, vv)

enumAttr :: [AttrName]
enumAttr = map (AttrNameNormal . ('-' :) . show) [1 :: Int ..]

isTermLeaf :: B.TTree -> Bool
isTermLeaf (B.TreeL token) = B.isTermToken token
isTermLeaf _               = False


-- ----------------------  Attribute name

-- | Attribute name for relmap operator.
data AttrName
    = AttrNameNormal String    -- ^ Normal attribute
    | AttrNameRelmap String    -- ^ Attribute for subrelmap
    | AttrNameLocal  String    -- ^ Attribute for local relation variable
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Test attribute name is for subrelmap.
isAttrNameRelmap :: AttrName -> Bool
isAttrNameRelmap (AttrNameRelmap _)  = True
isAttrNameRelmap _                   = False

-- | Test attribute name is for local variable.
isAttrNameLocal :: AttrName -> Bool
isAttrNameLocal (AttrNameLocal _)    = True
isAttrNameLocal _                    = False

-- | String part of attribute names.
attrNameText :: AttrName -> String
attrNameText (AttrNameNormal n)  = n
attrNameText (AttrNameRelmap n)  = n
attrNameText (AttrNameLocal  n)  = n

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrNameNormal "@trunk"
