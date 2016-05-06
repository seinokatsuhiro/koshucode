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
import qualified Koshucode.Baala.Syntax.Token          as S
import qualified Koshucode.Baala.Syntax.Attr.AttrPos   as S
import qualified Koshucode.Baala.Syntax.Attr.Para      as S
import qualified Koshucode.Baala.Syntax.Attr.Message   as Msg


-- ----------------------  Attribute layout

-- | Attribute layout.
data AttrLayout = AttrLayout
    { attrPosSorter   :: S.AttrSortTree     -- ^ Sorter for positional attributes
                                            --   (derived from @attrPos@)
    , attrClassifier  :: B.Map S.AttrName   -- ^ Attribute classifier
                                            --   (derived from @attrNamesP@ and @attrNamesN@)
    , attrPos         :: S.AttrNamePos      -- ^ Positional attribute
    , attrNamesP      :: [S.AttrName]       -- ^ Names of positional attributes
                                            --   (derived from @attrPos@)
    , attrNamesN      :: [S.AttrName]       -- ^ Names of named attributes
    }

instance Show AttrLayout where
    show AttrLayout {..} =
        "AttrLayout { positional = " ++ show attrPos
                ++ ", named = " ++ show attrNamesN ++ " }"

-- | Construct attribute layout from positional and named attributes.
attrLayout :: S.AttrNamePos -> [S.AttrName] -> AttrLayout
attrLayout pos namesN = sorter where
    sorter     = AttrLayout sorterP classify pos namesP namesN
    sorterP    = S.sortAttrTree pos
    namesP     = S.attrPosNameList pos
    classify   = attrClassify namesP namesN

attrClassify :: [S.AttrName] -> [S.AttrName] -> B.Map S.AttrName
attrClassify namesP namesN n = n2 where
    n2 :: S.AttrName
    n2 = let nam = S.attrNameText n
         in case lookup nam pairs of
              Just k  -> k
              Nothing -> n

    pairs    :: [B.Named S.AttrName]
    pairs    = map pair alls
    pair k   = (S.attrNameText k, k)
    alls     = S.attrNameTrunk : namesP ++ namesN


-- ----------------------  Attribute sorter

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> let a = attrLayout (S.AttrPos2 (S.AttrNormal "a") (S.AttrNormal "b")) [S.AttrNormal "x", S.AttrNormal "y"]
--   >>> attrSetSort a =<< S.tt "a b -x /c 'd -y e"
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

-- | Attribute set.
type AttrSet = S.Para S.AttrName S.TTree

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSetSort = [S.TTree] -> B.Ab AttrSet

-- | Sort attributes.
attrSetSort :: AttrLayout -> AttrSetSort
attrSetSort def = attrSetSortNamed B.>=> attrSetSortPos def

-- | Sort named part of attribute.
attrSetSortNamed :: AttrSetSort
attrSetSortNamed trees =
    do let p   = S.para maybeSingleHyphen trees
           p2  = S.paraNameAdd "@trunk" (S.paraPos p) p
           dup = S.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ S.paraNameMapKeys S.AttrNormal p2

-- | Sort positional part of attribute.
attrSetSortPos :: AttrLayout -> B.AbMap AttrSet
attrSetSortPos (AttrLayout sorter classify _ pos named) p =
    do let noPos      = null $ S.paraPos p
           nameList   = map fst $ S.paraNameList p
           overlapped = pos `B.overlap` nameList

       p2            <- case noPos && overlapped of
                          True  -> Right p
                          False -> S.paraPosName sorter p

       let p3         = S.paraNameMapKeys classify p2
           attr       = attrList p3

       attrCheck pos named attr
       Right p3

attrCheck :: [S.AttrName] -> [S.AttrName] -> [S.AttrTree] -> B.Ab ()
attrCheck namesP namesN attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ ('-' : u)
    where
      textAll = "@trunk" : t namesP ++ t namesN
      t       = map S.attrNameText

attrList :: S.Para n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . S.paraNameList

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: S.TTreeTo (Maybe String)
maybeSingleHyphen (S.TextLeafAttr _ n)      = Just n
maybeSingleHyphen _                         = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: S.TTreeTo (Maybe String)
maybeDoubleHyphen (S.TextLeafAttr2 _ n)     = Just n
maybeDoubleHyphen _                         = Nothing

