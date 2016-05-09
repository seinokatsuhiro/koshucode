{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Syntax.Attr.Attr
  ( -- * Attribute layout
    AttrParaSpec,
    AttrLayout (..),
    AttrBranch (..),
    attrLayout, attrBranch,
  
    -- * Attribute parameter
    AttrPara, AttrParaSort,
    attrParaSort, attrParaSortNamed,
    maybeSingleHyphen,
    maybeDoubleHyphen,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Para           as S
import qualified Koshucode.Baala.Syntax.Token          as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName  as S
import qualified Koshucode.Baala.Syntax.Attr.Message   as Msg


-- ----------------------  Attribute layout

-- | Parameter specification for attribute name.
type AttrParaSpec = S.ParaSpec S.AttrName

-- | Attribute layout.
data AttrLayout = AttrLayout [(Maybe String, AttrBranch)]
                  deriving (Show)

data AttrBranch = AttrBranch
    { attrParaSpec    :: AttrParaSpec       -- ^ Parameter specification
    , attrClassifier  :: B.Map S.AttrName   -- ^ Attribute classifier
    }

instance Show AttrBranch where
    show AttrBranch {..} = "AttrBranch (" ++ show attrParaSpec ++ ")"

attrLayout :: AttrParaSpec -> AttrLayout
attrLayout spec = AttrLayout [(Nothing, attrBranch spec)]

-- | Construct attribute layout from positional and named attributes.
attrBranch :: AttrParaSpec -> AttrBranch
attrBranch spec = AttrBranch spec $ attrClassify spec

-- | Set type of attribute name.
attrClassify :: AttrParaSpec -> B.Map S.AttrName
attrClassify spec n = n' where
    n' = case lookup (S.attrNameText n) pairs of
           Just k  -> k
           Nothing -> n

    pairs   :: [(String, S.AttrName)]
    pairs   = map pair $ S.paraSpecNames spec
    pair k  = (S.attrNameText k, k)


-- ----------------------  Attribute sorter

-- | Attribute parameter.
type AttrPara = S.Para S.AttrName S.TTree

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrParaSort = [S.TTree] -> B.Ab AttrPara

-- | Sort attributes.
attrParaSort :: AttrLayout -> AttrParaSort
attrParaSort lay = attrParaSortNamed B.>=> attrParaSortPos lay

-- | Sort named part of attribute.
attrParaSortNamed :: AttrParaSort
attrParaSortNamed trees =
    do let p = S.para byHyphen trees
       Right $ S.paraNameAdd S.attrNameTrunk (S.paraPos p) p

-- | Sort positional part of attribute.
attrParaSortPos :: AttrLayout -> B.AbMap AttrPara
attrParaSortPos (AttrLayout branches) p = loop [] branches where
    loop us [] = Msg.unexpAttrMulti $ map (fmap S.attrNameCode) $ reverse us
    loop us ((_, b) : bs) =
        case branch b of
          Right p' -> Right p'
          Left u   -> loop (u:us) bs

    branch (AttrBranch spec classify) =
        S.paraMatch spec $ S.paraNameMapKeys classify p

byHyphen :: S.TTreeTo (Maybe S.AttrName)
byHyphen = fmap S.AttrNormal . maybeSingleHyphen

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: S.TTreeTo (Maybe String)
maybeSingleHyphen (S.TextLeafAttr _ n)      = Just n
maybeSingleHyphen _                         = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: S.TTreeTo (Maybe String)
maybeDoubleHyphen (S.TextLeafAttr2 _ n)     = Just n
maybeDoubleHyphen _                         = Nothing

