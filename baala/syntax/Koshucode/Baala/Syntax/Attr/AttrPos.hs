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
    attrNameText, attrNameCode,
    attrNameTrunk,

    -- * Positional name
    AttrTree,
  ) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Syntax.Token         as S


-- ----------------------  Positional attribute

-- | Positional attribute.
data AttrPos a
    = AttrPos0            -- ^ No attributes
    | AttrPos1 a          -- ^ Single attribute
    | AttrPos2 a a        -- ^ Two attributes
    | AttrPos3 a a a      -- ^ Three attributes
    | AttrPos4 a a a a    -- ^ Four attributes
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

-- | Attribute string with hyphen.
attrNameCode :: AttrName -> String
attrNameCode = ('-':) . attrNameText

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrNormal "@trunk"


-- --------------------------------------------  Positional name

-- | Attribute name and its contents.
type AttrTree = (AttrName, [S.TTree])

