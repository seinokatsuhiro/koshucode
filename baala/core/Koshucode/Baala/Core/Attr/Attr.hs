{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Attr.Attr
  ( -- * Attribute layout
    AttrSorter (..),
    attrSorter,
  
    -- * Attribute set
    AttrSet, AttrSetSort,
    attrSetSort, attrSetSortNamed,
    maybeSingleHyphen,
    maybeDoubleHyphen,
    -- $AttributeSorter
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as D
import qualified Koshucode.Baala.Core.Attr.AttrPos   as C
import qualified Koshucode.Baala.Core.Attr.Message   as Msg


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data AttrSorter = AttrSorter
    { attrPosSorter   :: C.AttrSortTree     -- ^ Sorter for positional attributes
                                            --   (derived from @attrPos@)
    , attrClassifier  :: B.Map C.AttrName   -- ^ Attribute classifier
                                            --   (derived from @attrNamesP@ and @attrNamesN@)
    , attrPos         :: C.AttrNamePos      -- ^ Positional attribute
    , attrNamesP      :: [C.AttrName]       -- ^ Names of positional attributes
                                            --   (derived from @attrPos@)
    , attrNamesN      :: [C.AttrName]       -- ^ Names of named attributes
    }

instance Show AttrSorter where
    show AttrSorter {..} =
        "AttrSorter { positional = " ++ show attrPos
                ++ ", named = " ++ show attrNamesN ++ " }"

-- | Construct attribute-sorting specification.
attrSorter :: C.AttrNamePos -> [C.AttrName] -> AttrSorter
attrSorter pos namesN = sorter where
    sorter     = AttrSorter sorterP classify pos namesP namesN
    sorterP    = C.sortAttrTree pos
    namesP     = C.attrPosNameList pos
    classify   = attrClassify namesP namesN

attrClassify :: [C.AttrName] -> [C.AttrName] -> B.Map C.AttrName
attrClassify namesP namesN n = n2 where
    n2 :: C.AttrName
    n2 = let nam = C.attrNameText n
         in case lookup nam pairs of
              Just k  -> k
              Nothing -> n

    pairs    :: [B.Named C.AttrName]
    pairs    = map pair alls
    pair k   = (C.attrNameText k, k)
    alls     = C.attrNameTrunk : namesP ++ namesN


-- ----------------------  Attribute sorter

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> let a = attrSorter (C.AttrPos2 (C.AttrNormal "a") (C.AttrNormal "b")) [C.AttrNormal "x", C.AttrNormal "y"]
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
type AttrSet = D.ParaBody C.AttrName D.TTree

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSetSort = [D.TTree] -> B.Ab AttrSet

-- | Sort attributes.
attrSetSort :: AttrSorter -> AttrSetSort
attrSetSort def = attrSetSortNamed B.>=> attrSetSortPos def

-- | Sort named part of attribute.
attrSetSortNamed :: AttrSetSort
attrSetSortNamed trees =
    do let p   = D.para maybeSingleHyphen trees
           p2  = D.paraNameAdd "@trunk" (D.paraPos p) p
           dup = D.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ D.paraNameMapKeys C.AttrNormal p2

-- | Sort positional part of attribute.
attrSetSortPos :: AttrSorter -> B.AbMap AttrSet
attrSetSortPos (AttrSorter sorter classify _ pos named) p =
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

attrCheck :: [C.AttrName] -> [C.AttrName] -> [C.AttrTree] -> B.Ab ()
attrCheck namesP namesN attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ ('-' : u)
    where
      textAll = "@trunk" : t namesP ++ t namesN
      t       = map C.attrNameText

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

