{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Lexmap.Attribute
  ( -- * Attribute name
    AttrName (..),
    isAttrNameRelmap, isAttrNameLocal,
    attrNameText,
    attrNameTrunk,
  
    -- * Attribute trees
    AttrDefine (..),
    AttrTree, AttrPara,
    AttrSortTree, AttrSortPara,
  
    -- * Attribute sorter
    attrSort,
    attrBranch,
    maybeHyname,
    -- $AttributeSorter
  ) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Msg


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


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data AttrDefine = AttrDefine
    { attrTrunkSorter  :: AttrSortTree       -- Trunk sorter
    , attrClassifier   :: B.Map AttrName     -- Attribute classifier
    , attrTrunkNames   :: [AttrName]         -- Trunk names
    , attrBranchNames  :: [AttrName]         -- Branch names
    }

-- | Attribute name and its contents.
type AttrTree = (AttrName, [B.TTree])

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSortPara = [B.TTree] -> B.Ab AttrPara

type AttrSortTree = [B.TTree] -> B.Ab [AttrTree]

type AttrPara = B.ParaBody AttrName B.TTree


-- ----------------------  Attribute sorter

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> Right . hyphenAssc =<< B.tt "a b -x /c 'd -y e"
--   [ ("@trunk", [TreeL (TText 1 0 "a"), TreeL (TText 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TText 9 1 "d")])
--   , ("-y", [TreeL (TText 14 0 "e")]) ]

attrSort :: AttrDefine -> AttrSortPara
attrSort def = attrBranch B.>=> attrTrunk def

attrBranch :: AttrSortPara
attrBranch trees =
    do let p   = B.para maybeHyname trees
           p2  = B.paraNameAdd "@trunk" (B.paraPos p) p
           dup = B.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.paraNameMapKeys AttrNameNormal p2

maybeHyname :: B.TTreeTo (Maybe String)
maybeHyname (B.TextLeafRaw _ n@('-' : _))  = Just n
maybeHyname _                              = Nothing

attrTrunk :: AttrDefine -> B.AbMap AttrPara
attrTrunk (AttrDefine sorter classify pos named) p =
    do let noPos      = null $ B.paraPos p
           nameList   = map fst $ B.paraNameList p
           overlapped = pos `B.overlap` nameList

       p2            <- case noPos && overlapped of
                          True  -> Right p
                          False -> B.paraPosName sorter p

       let p3         = B.paraNameMapKeys classify p2
           attr       = attrList p3

       attrCheck pos named attr
       Right p3

attrCheck :: [AttrName] -> [AttrName] -> [AttrTree] -> B.Ab ()
attrCheck trunkNames branchNames attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ u
    where
      textAll = "@trunk" : t trunkNames ++ t branchNames
      t       = map attrNameText

attrList :: B.ParaBody n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . B.paraNameList

