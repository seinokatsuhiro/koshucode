{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of Relmap operator.

module Koshucode.Baala.Core.Lexmap.Attribute
( -- * Attribute name
  AttrName (..),
  attrNameText,
  attrNameAttr,
  attrNameTrunk,
  isAttrRelmap,

  -- * Data type
  Roa,
  Roal,
  RoaSpec,
  RoaSorter,
  TreeSorter,

  -- * Branch sorter
  roaFrom,
  roaBranch,
  roaSorter,
  -- $BranchSorter

) where

import qualified Data.Generics                as G
import qualified Data.List                    as List
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Message


-- ----------------------  Attribute name

data AttrName
    = AttrTree   String
    | AttrRelmap String
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

attrNameText :: AttrName -> String
attrNameText (AttrTree   text) = text
attrNameText (AttrRelmap text) = text

attrNameTrunk :: AttrName
attrNameTrunk = AttrTree "@trunk"

attrNameAttr :: AttrName
attrNameAttr = AttrTree "@attr"

isAttrRelmap :: AttrName -> Bool
isAttrRelmap (AttrRelmap _) = True
isAttrRelmap _              = False


-- ----------------------  Data type

-- | Relmap operation attributes as association list.
type Roa = [(AttrName, [B.TokenTree])]

-- | Association of operator use and something.
--   Operator use is represented as pair of operator name and attributes.
type Roal a = ((String, Roa), a)

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type RoaSorter = [B.TokenTree] -> B.Ab Roa

type TreeSorter = [B.TokenTree] -> B.Ab [B.NamedTrees]

-- | Attribute sorter for relmap operator.
--   It consists of trunk sorter, trunk names, and branch names.
type RoaSpec =
    ( B.AbMap Roa  -- Trunk sorter
    , [AttrName]   -- Trunk names
    , [AttrName]   -- Branch names
    )


-- ----------------------  Branch sorter

-- $BranchSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> Right . roaFrom =<< B.tt "a b -x /c 'd -y e"
--   [ ("@trunk", [TreeL (TText 1 0 "a"), TreeL (TText 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TText 9 1 "d")])
--   , ("-y", [TreeL (TText 14 0 "e")]) ]

roaSorter :: RoaSpec -> RoaSorter
roaSorter spec = roaBranch B.>=> roaTrunk spec

roaBranch :: RoaSorter
roaBranch trees =
    do roa <- roaFrom trees
       let dup = B.duplicates $ map fst roa
       B.when (B.notNull dup) $ Message.unexpAttr $ "Duplicate " ++ unwords dup
       Right $ B.mapFstTo AttrTree roa

roaFrom :: TreeSorter
roaFrom = Right . B.assocBy name "@trunk" where
    name (B.TreeL (B.TText _ 0 n@('-' : _))) = Just n
    name _ = Nothing

roaTrunk :: RoaSpec -> B.AbMap Roa
roaTrunk spec@(trunkSorter, trunkNames, _) roa = roa3 where
    roa3, roa2 :: B.Ab Roa
    roa3 = attrClassify spec =<< roa2
    roa2 | B.notNull wrap = Right roa
         | otherwise      = trunkSorter roa

    wrap, given :: [AttrName]
    wrap  = given `List.intersect` trunkNames
    given = map fst roa

attrClassify :: RoaSpec -> B.AbMap Roa
attrClassify (_, trunkNames, branchNames) roa = roa2 where
    roa2     :: B.Ab Roa
    roa2     = B.sequenceFst $ B.mapFstTo relmap roa

    relmap :: B.AbMap AttrName
    relmap n = let name = attrNameText n
               in case lookup name pairs of
                 Just k  -> Right k
                 Nothing -> Message.unexpAttr $ "Unknown " ++ name

    pairs    :: [B.Named AttrName]
    pairs    = map pair alls
    pair n   = (attrNameText n, n)
    alls     = attrNameTrunk : trunkNames ++ branchNames

