{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Positional attributes.

module Koshucode.Baala.Core.Lexmap.Sorter
  ( AttrPos (..),
    ropAttrDef,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute as C
import qualified Koshucode.Baala.Core.Message          as Msg


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

ropAttrPos :: AttrPos C.AttrName -> C.AttrSortTree
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

enumAttr :: [C.AttrName]
enumAttr = map (C.AttrNameNormal . ('-' :) . show) [1 :: Int ..]

isTermLeaf :: B.TTree -> Bool
isTermLeaf (B.TreeL token) = B.isTermToken token
isTermLeaf _               = False

ropAttrDef :: AttrPos C.AttrName -> [C.AttrName] -> C.AttrDefine
ropAttrDef attrType branchNames =
    C.AttrDefine trunkSorter classify trunkNames branchNames where
        trunkSorter = ropAttrPos attrType
        trunkNames  = attrTypeNames attrType
        classify    = attrClassify trunkNames branchNames

attrClassify :: [C.AttrName] -> [C.AttrName] -> B.Map C.AttrName
attrClassify trunkNames branchNames n = n2 where
    n2 :: C.AttrName
    n2 = let nam = C.attrNameText n
         in case lookup nam pairs of
              Just k  -> k
              Nothing -> n

    pairs    :: [B.Named C.AttrName]
    pairs    = map pair alls
    pair k   = (C.attrNameText k, k)
    alls     = C.attrNameTrunk : trunkNames ++ branchNames
