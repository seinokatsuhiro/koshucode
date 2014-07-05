{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Relmap operator attributes.

module Koshucode.Baala.Core.Lexmap.Attribute
( -- * Data type
  Roa,
  Roal,
  RoaSorter,
  RoaSpec,
  AttrName (..),
  attrNameText,
  attrNameAttr,
  attrNameTrunk,

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



-- ----------------------  Data type

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

-- | Relmap operation attributes as association list.
type Roa = [(AttrName, [B.TokenTree])]

-- | Association of operator use and something.
--   Operator use is represented as pair of operator name and attributes.
type Roal a = ((String, Roa), a)

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type RoaSorter = [B.TokenTree] -> B.Ab Roa

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

roaFrom :: RoaSorter
roaFrom = Right . B.assocBy attrhName attrNameTrunk where
    attrhName (B.TreeL (B.TText _ 0 n@('-' : _))) = Just $ AttrTree n
    attrhName _ = Nothing

roaSorter :: RoaSpec -> RoaSorter
roaSorter spec = roaBranch B.>=> roaTrunk spec

roaBranch :: RoaSorter
roaBranch trees =
    do roa <- roaFrom trees
       let dup  = B.duplicates $ map fst roa
           dupt = map attrNameText dup
       B.when (B.notNull dup) $ Message.unexpAttr $ "Duplicate " ++ unwords dupt
       Right roa

roaTrunk :: RoaSpec -> B.AbMap Roa
roaTrunk (trunkSorter, trunkNames, branchNames) roa = sorted where
    alls, given, unk, wrap :: [AttrName]
    alls  = attrNameTrunk : trunkNames ++ branchNames
    given = map fst roa
    unk   = given  List.\\  alls
    unkt  = map attrNameText unk
    wrap  = given `List.intersect` trunkNames

    sorted :: B.Ab Roa
    sorted | B.notNull unk  = Message.unexpAttr $ "Unknown " ++ unwords unkt
           | B.notNull wrap = Right roa
           | otherwise      = trunkSorter roa

