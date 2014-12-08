{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Lexmap.Attribute
  ( -- * Attribute name
    AttrName (..),
    isAttrRelmap,
    attrNameText,
    attrNameAttr,
    attrNameTrunk,
  
    -- * Attribute trees
    AttrDefine (..),
    AttrTrees,
    AttrSort,
    TreeSort,
    RopName,
    RelmapKey,
  
    -- * Attribute sorter
    attrSort,
    attrSortBranch,
    hyphenAssc,
    -- $AttributeSorter
  ) where

import qualified Data.Generics                as G
import qualified Data.List                    as List
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Msg


-- ----------------------  Attribute name

data AttrName
    = AttrTree   String
    | AttrRelmap String
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Test attribute name is for relmap.
isAttrRelmap :: AttrName -> Bool
isAttrRelmap (AttrRelmap _) = True
isAttrRelmap _              = False

-- | String part of attribute names.
attrNameText :: AttrName -> String
attrNameText (AttrTree   text) = text
attrNameText (AttrRelmap text) = text

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrTree "@trunk"

-- | Constant for attribute name @\@attr@.
attrNameAttr :: AttrName
attrNameAttr = AttrTree "@attr"


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data AttrDefine = AttrDefine
    { attrTrunkSorter :: B.AbMap AttrTrees   -- Trunk sorter
    , attrClassifier  :: B.AbMap AttrTrees   -- Attribute classifier
    , attrTrunkNames  :: [AttrName]          -- Trunk names
    , attrBranchNames :: [AttrName]          -- Branch names
    }

-- | List of attribute name and its contents.
type AttrTrees = [ (AttrName, [B.TTree]) ]

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSort = [B.TTree] -> B.Ab AttrTrees

type TreeSort = [B.TTree] -> [B.NamedTrees]

-- | Name of relmap operator.
type RopName = String

-- | Search key for 'Lexmap' or 'Relmap'.
type RelmapKey = (RopName, AttrTrees)


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
attrSort spec = attrSortBranch B.>=> attrTrunk spec

attrSortBranch :: AttrSort
attrSortBranch trees =
    do let assc = hyphenAssc trees
           dup  = B.duplicates $ map fst assc
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.mapFstTo AttrTree assc

hyphenAssc :: TreeSort
hyphenAssc = B.assocBy name "@trunk" where
    name (B.TreeL (B.TText _ B.TextRaw n@('-' : _))) = Just n
    name _ = Nothing

attrTrunk :: AttrDefine -> B.AbMap AttrTrees
attrTrunk (AttrDefine sorter classify trunkNames _) roa = roa3 where
    roa3, roa2 :: B.Ab AttrTrees
    roa3 = classify =<< roa2
    roa2 | B.notNull wrap = Right  roa
         | otherwise      = sorter roa

    wrap, given :: [AttrName]
    wrap  = given `List.intersect` trunkNames
    given = map fst roa

