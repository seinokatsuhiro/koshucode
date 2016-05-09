{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Syntax.Attr.Attr
  ( -- * Attribute layout
    AttrParaSpec,
    AttrLayout (..),
    attrLayout,
  
    -- * Attribute parameter
    AttrPara, AttrParaSort,
    attrParaSort, attrParaSortNamed,
    maybeSingleHyphen,
    maybeDoubleHyphen,
    -- $AttributeSorter
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
data AttrLayout = AttrLayout
    { attrParaSpec    :: AttrParaSpec       -- ^ Parameter specification
    , attrClassifier  :: B.Map S.AttrName   -- ^ Attribute classifier
    }

instance Show AttrLayout where
    show AttrLayout {..} = "AttrLayout (" ++ show attrParaSpec ++ ")"

-- | Construct attribute layout from positional and named attributes.
attrLayout :: AttrParaSpec -> AttrLayout
attrLayout spec = AttrLayout spec $ attrClassify spec

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

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> let a = attrLayout (S.AttrPos2 (S.AttrNormal "a") (S.AttrNormal "b")) [S.AttrNormal "x", S.AttrNormal "y"]
--   >>> attrParaSort a =<< S.tt "a b -x /c 'd -y e"
--   Right (Para {
--     paraAll  = [ TreeL (TText CodePt {..} TextRaw "a"),
--                  TreeL (TText CodePt {..} TextRaw "b"),
--                  TreeL (TText CodePt {..} TextRaw "-x"),
--                  TreeL (TTermN CodePt {..} "c"),
--                  TreeL (TText CodePt {..} TextQ "d"),
--                  TreeL (TText CodePt {..} TextRaw "-y"),
--                  TreeL (TText CodePt {..} TextRaw "e") ],
--     paraPos  = [ TreeL (TText CodePt {..} TextRaw "a"),
--                  TreeL (TText CodePt {..} TextRaw "b") ],
--     paraName = fromList [ (AttrNormal "@trunk",
--                                            [[TreeL (TText CodePt {..} TextRaw "a"),
--                                              TreeL (TText CodePt {..} TextRaw "b")]]),
--                           (AttrNormal "a", [[TreeL (TText CodePt {..} TextRaw "a")]]),
--                           (AttrNormal "b", [[TreeL (TText CodePt {..} TextRaw "b")]]),
--                           (AttrNormal "x", [[TreeL (TTermN CodePt {..} "c"),
--                                              TreeL (TText CodePt {..} TextQ "d")]]),
--                           (AttrNormal "y", [[TreeL (TText CodePt {..} TextRaw "e")]]) ]
--     })

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
attrParaSortPos (AttrLayout spec classify) p =
    case S.paraMatch spec $ S.paraNameMapKeys classify p of
      Right p' -> Right p'
      Left u -> Msg.unexpAttr' $ fmap S.attrNameCode u

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

