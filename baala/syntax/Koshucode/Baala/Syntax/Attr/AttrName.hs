{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute name.

module Koshucode.Baala.Syntax.Attr.AttrName
  ( AttrName (..),
    isAttrNameRelmap,
    attrNameText, attrNameCode,
    attrNameTrunk,
  ) where

import qualified Data.Generics                        as G

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

