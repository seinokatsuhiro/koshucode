{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Lexmap.Attribute
  ( -- * Attribute name
    AttrName (..),
    isAttrNameRelmap, isAttrNameNest,
    attrNameText,
    attrNameAttr, attrNameTrunk,
  
    -- * Attribute trees
    AttrDefine (..),
    AttrTree,
    AttrSort,
    AttrPara,
    RopName,
  
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
    | AttrNameNest   String    -- ^ Attribute for nested relation reference
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Test attribute name is for subrelmap.
isAttrNameRelmap :: AttrName -> Bool
isAttrNameRelmap (AttrNameRelmap _)  = True
isAttrNameRelmap _                   = False

-- | Test attribute name is for nested relation reference.
isAttrNameNest :: AttrName -> Bool
isAttrNameNest (AttrNameNest _)      = True
isAttrNameNest _                     = False

-- | String part of attribute names.
attrNameText :: AttrName -> String
attrNameText (AttrNameNormal n)  = n
attrNameText (AttrNameRelmap n)  = n
attrNameText (AttrNameNest   n)  = n

-- | Constant for attribute name @\@attr@.
attrNameAttr :: AttrName
attrNameAttr = AttrNameNormal "@attr"

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrNameNormal "@trunk"


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data AttrDefine = AttrDefine
    { attrTrunkSorter :: AttrSort           -- Trunk sorter
    , attrClassifier  :: B.Map AttrName     -- Attribute classifier
    , attrTrunkNames  :: [AttrName]         -- Trunk names
    , attrBranchNames :: [AttrName]         -- Branch names
    }

-- | List of attribute name and its contents.
type AttrTree = (AttrName, [B.TTree])

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSort = [B.TTree] -> B.Ab [AttrTree]

type AttrPara = B.ParaBody AttrName B.TTree

-- | Name of relmap operator.
type RopName = String


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

attrSort :: AttrDefine -> AttrSort
attrSort def = attrBranch2 B.>=> attrTrunk def

attrBranch :: AttrSort
attrBranch trees =
    do let p = B.para maybeHyname trees
           assc = ("@trunk", B.paraPos p) : attrList p
           dup  = B.duplicates $ map fst assc
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.mapFstTo AttrNameNormal assc

attrBranch2 :: [B.TTree] -> B.Ab AttrPara
attrBranch2 trees =
    do let p   = B.para maybeHyname trees
           p2  = B.paraNameAdd "@trunk" (B.paraPos p) p
           dup = B.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.paraNameMapKeys AttrNameNormal p2

maybeHyname :: B.TTreeTo (Maybe String)
maybeHyname (B.TextLeafRaw _ n@('-' : _))  = Just n
maybeHyname _                              = Nothing

attrCheck :: [AttrName] -> [AttrName] -> [AttrTree] -> B.Ab [AttrTree]
attrCheck trunkNames branchNames attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right attr
      u : _ -> Msg.unexpAttr $ "Unknown " ++ u
    where
      textAll = "@attr" : "@trunk" : t trunkNames ++ t branchNames
      t       = map attrNameText

attrTrunk :: AttrDefine -> AttrPara -> B.Ab [AttrTree]
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

attrList :: B.ParaBody n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . B.paraNameList

