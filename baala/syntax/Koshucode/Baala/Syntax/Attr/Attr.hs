{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Syntax.Attr.Attr
  ( -- * Attribute layout
    AttrLayout (..),
    attrLayout,
  
    -- * Attribute set
    AttrSet, AttrSetSort,
    attrSetSort, attrSetSortNamed,
    maybeSingleHyphen,
    maybeDoubleHyphen,
    -- $AttributeSorter
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Token          as D
import qualified Koshucode.Baala.Syntax.Attr.AttrPos   as D
import qualified Koshucode.Baala.Syntax.Attr.Para      as D
import qualified Koshucode.Baala.Syntax.Attr.Message   as Msg


-- ----------------------  Attribute layout

-- | Attribute layout.
data AttrLayout = AttrLayout
    { attrPosSorter   :: D.AttrSortTree     -- ^ Sorter for positional attributes
                                            --   (derived from @attrPos@)
    , attrClassifier  :: B.Map D.AttrName   -- ^ Attribute classifier
                                            --   (derived from @attrNamesP@ and @attrNamesN@)
    , attrPos         :: D.AttrNamePos      -- ^ Positional attribute
    , attrNamesP      :: [D.AttrName]       -- ^ Names of positional attributes
                                            --   (derived from @attrPos@)
    , attrNamesN      :: [D.AttrName]       -- ^ Names of named attributes
    }

instance Show AttrLayout where
    show AttrLayout {..} =
        "AttrLayout { positional = " ++ show attrPos
                ++ ", named = " ++ show attrNamesN ++ " }"

-- | Construct attribute layout from positional and named attributes.
attrLayout :: D.AttrNamePos -> [D.AttrName] -> AttrLayout
attrLayout pos namesN = sorter where
    sorter     = AttrLayout sorterP classify pos namesP namesN
    sorterP    = D.sortAttrTree pos
    namesP     = D.attrPosNameList pos
    classify   = attrClassify namesP namesN

attrClassify :: [D.AttrName] -> [D.AttrName] -> B.Map D.AttrName
attrClassify namesP namesN n = n2 where
    n2 :: D.AttrName
    n2 = let nam = D.attrNameText n
         in case lookup nam pairs of
              Just k  -> k
              Nothing -> n

    pairs    :: [B.Named D.AttrName]
    pairs    = map pair alls
    pair k   = (D.attrNameText k, k)
    alls     = D.attrNameTrunk : namesP ++ namesN


-- ----------------------  Attribute sorter

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> let a = attrLayout (D.AttrPos2 (D.AttrNormal "a") (D.AttrNormal "b")) [D.AttrNormal "x", D.AttrNormal "y"]
--   >>> attrSetSort a =<< D.tt "a b -x /c 'd -y e"
--   Right (ParaBody {
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

-- | Attribute set.
type AttrSet = D.ParaBody D.AttrName D.TTree

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSetSort = [D.TTree] -> B.Ab AttrSet

-- | Sort attributes.
attrSetSort :: AttrLayout -> AttrSetSort
attrSetSort def = attrSetSortNamed B.>=> attrSetSortPos def

-- | Sort named part of attribute.
attrSetSortNamed :: AttrSetSort
attrSetSortNamed trees =
    do let p   = D.para maybeSingleHyphen trees
           p2  = D.paraNameAdd "@trunk" (D.paraPos p) p
           dup = D.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ D.paraNameMapKeys D.AttrNormal p2

-- | Sort positional part of attribute.
attrSetSortPos :: AttrLayout -> B.AbMap AttrSet
attrSetSortPos (AttrLayout sorter classify _ pos named) p =
    do let noPos      = null $ D.paraPos p
           nameList   = map fst $ D.paraNameList p
           overlapped = pos `B.overlap` nameList

       p2            <- case noPos && overlapped of
                          True  -> Right p
                          False -> D.paraPosName sorter p

       let p3         = D.paraNameMapKeys classify p2
           attr       = attrList p3

       attrCheck pos named attr
       Right p3

attrCheck :: [D.AttrName] -> [D.AttrName] -> [D.AttrTree] -> B.Ab ()
attrCheck namesP namesN attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ ('-' : u)
    where
      textAll = "@trunk" : t namesP ++ t namesN
      t       = map D.attrNameText

attrList :: D.ParaBody n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . D.paraNameList

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: D.TTreeTo (Maybe String)
maybeSingleHyphen (D.TextLeafAttr _ n)      = Just n
maybeSingleHyphen _                         = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: D.TTreeTo (Maybe String)
maybeDoubleHyphen (D.TextLeafAttr2 _ n)     = Just n
maybeDoubleHyphen _                         = Nothing

