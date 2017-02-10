{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute name.

module Koshucode.Baala.Syntax.Attr.AttrName
  ( AttrName (..),
    isAttrNameRelmap,
    attrNameText, attrNameCode,
    attrNameTrunk,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Syntax.Symbol     as S

-- | Attribute name of relmap.
data AttrName
    = AttrNormal       S.Chars  -- ^ Normal attribute
    | AttrRelmapNormal S.Chars  -- ^ Attribute for submap
    | AttrRelmapLocal  S.Chars  -- ^ Attribute for submap with local relation references
      deriving (Show, Eq, Ord)

-- | Test attribute name is for subrelmap.
isAttrNameRelmap :: AttrName -> Bool
isAttrNameRelmap (AttrRelmapNormal _)  = True
isAttrNameRelmap (AttrRelmapLocal  _)  = True
isAttrNameRelmap _                     = False

-- | String part of attribute names.
attrNameText :: AttrName -> S.Chars
attrNameText (AttrNormal       n)  = n
attrNameText (AttrRelmapNormal n)  = n
attrNameText (AttrRelmapLocal  n)  = n

-- | Attribute string with hyphen.
attrNameCode :: AttrName -> S.Chars
attrNameCode = ('-' O.<:>) . attrNameText

-- | Constant for attribute name @\@trunk@.
attrNameTrunk :: AttrName
attrNameTrunk = AttrNormal "@trunk"

