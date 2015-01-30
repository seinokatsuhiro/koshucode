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
    RopName,
  
    -- * Attribute sorter
    attrSort,
    attrBranch,
    hyphenAssc,
    -- $AttributeSorter
  ) where

import qualified Data.Generics                as G
import qualified Data.List                    as List
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
attrSort def = attrBranch B.>=> attrTrunk def

attrBranch :: AttrSort
attrBranch trees =
    do let assc = hyphenAssc trees
           dup  = B.duplicates $ map fst assc
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.mapFstTo AttrNameNormal assc

hyphenAssc :: [B.TTree] -> [B.NamedTrees]
hyphenAssc = B.assocBy name "@trunk" where
    name (B.TextLeafRaw _ n@('-' : _)) = Just n
    name _ = Nothing

attrTrunk :: AttrDefine -> B.AbMap [AttrTree]
attrTrunk (AttrDefine sorter classify trunkNames branchNames) attr = attr4 where
    attr4, attr3, attr2 :: B.Ab [AttrTree]
    attr4 = Right . B.mapFstTo classify =<< attr3
    attr3 = do a <- attr2
               case t (map fst a) B.\\ textAll of
                 []    -> attr2
                 u : _ -> Msg.unexpAttr $ "Unknown " ++ u
    attr2 | B.notNull wrap  = Right attr
          | otherwise       = case lookup attrNameTrunk attr of
                                Just xs -> Right . (++ attr) =<< sorter xs
                                Nothing -> Right attr

    t       = map attrNameText
    textAll = "@attr" : "@trunk" : t trunkNames ++ t branchNames

    wrap, given :: [AttrName]
    wrap  = given `List.intersect` trunkNames
    given = map fst attr

-- attrTrunk2 :: AttrDefine -> B.AbMap (B.ParaBody AttrName B.TTree)
-- attrTrunk2 (AttrDefine sorter classify trunkNames _) p = p2 where
--     p2 = B.paraPosName sorter p
