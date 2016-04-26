{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Lexmap.Attr
  ( -- * Attributes of relmap operator
    AttrSorter (..),
    attrSorter,
  
    -- * Attribute sorter
    AttrPara, AttrSortPara,
    attrSort, attrBranch,
    maybeSingleHyphen,
    maybeDoubleHyphen,
    -- $AttributeSorter
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Data                  as D
import qualified Koshucode.Baala.Core.Lexmap.AttrPos   as C
import qualified Koshucode.Baala.Core.Lexmap.Message   as Msg


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data AttrSorter = AttrSorter
    { attrPosSorter    :: C.AttrSortTree     -- ^ Sorter for positional attributes
    , attrClassifier   :: B.Map C.AttrName   -- ^ Attribute classifier
    , attrPos          :: C.AttrNamePos      -- ^ Positional attribute
    , attrPosNames     :: [C.AttrName]       -- ^ Names of positional attributes
    , attrBranchNames  :: [C.AttrName]       -- ^ Names of named attributes
    }

instance Show AttrSorter where
    show AttrSorter {..} =
        "AttrSorter { positional = " ++ show attrPos
                ++ ", named = " ++ show attrBranchNames ++ " }"

-- | Construct attribute-sorting specification.
attrSorter :: C.AttrNamePos -> [C.AttrName] -> AttrSorter
attrSorter pos branchNames = sorter where
    sorter       = AttrSorter posSorter classify pos posNames branchNames
    posSorter    = C.sortAttrTree pos
    posNames     = C.attrPosNameList pos
    classify     = attrClassify posNames branchNames

attrClassify :: [C.AttrName] -> [C.AttrName] -> B.Map C.AttrName
attrClassify posNames branchNames n = n2 where
    n2 :: C.AttrName
    n2 = let nam = C.attrNameText n
         in case lookup nam pairs of
              Just k  -> k
              Nothing -> n

    pairs    :: [B.Named C.AttrName]
    pairs    = map pair alls
    pair k   = (C.attrNameText k, k)
    alls     = C.attrNameTrunk : posNames ++ branchNames


-- ----------------------  Attribute sorter

-- $AttributeSorter
--
--   Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> let a = attrSorter (C.AttrPos2 (C.AttrNormal "a") (C.AttrNormal "b")) [C.AttrNormal "x", C.AttrNormal "y"]
--   >>> attrSort a =<< D.tt "a b -x /c 'd -y e"
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

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSortPara = [D.TTree] -> B.Ab AttrPara

type AttrPara = D.ParaBody C.AttrName D.TTree

attrSort :: AttrSorter -> AttrSortPara
attrSort def = attrBranch B.>=> attrSortPos def

attrBranch :: AttrSortPara
attrBranch trees =
    do let p   = D.para maybeSingleHyphen trees
           p2  = D.paraNameAdd "@trunk" (D.paraPos p) p
           dup = D.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ D.paraNameMapKeys C.AttrNormal p2

-- | Take out hyphened text (like @"-x"@) from token tree.
maybeSingleHyphen :: D.TTreeTo (Maybe String)
maybeSingleHyphen (D.TextLeafAttr _ n)      = Just n
maybeSingleHyphen _                         = Nothing

-- | Take out double-hyphened text (like @"--xyz"@) from token tree.
maybeDoubleHyphen :: D.TTreeTo (Maybe String)
maybeDoubleHyphen (D.TextLeafAttr2 _ n)     = Just n
maybeDoubleHyphen _                         = Nothing

attrSortPos :: AttrSorter -> B.AbMap AttrPara
attrSortPos (AttrSorter sorter classify _ pos named) p =
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
attrCheck posNames branchNames attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ ('-' : u)
    where
      textAll = "@trunk" : t posNames ++ t branchNames
      t       = map C.attrNameText

attrList :: D.ParaBody n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . D.paraNameList

