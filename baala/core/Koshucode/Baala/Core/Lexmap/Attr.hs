{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attributes of relmap operator.

module Koshucode.Baala.Core.Lexmap.Attr
  ( -- * Attributes of relmap operator
    RopAttr (..),
    ropAttrCons,
  
    -- * Attribute sorter
    AttrPara, AttrSortPara,
    attrSort, attrBranch,
    maybeSingleHyphen,
    -- $AttributeSorter
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap.AttrPos   as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- ----------------------  Attribute trees

-- | Definition of attribute sorter.
data RopAttr = RopAttr
    { attrPosSorter    :: C.AttrSortTree         -- Sorter for positional attributes
    , attrClassifier   :: B.Map C.AttrName       -- Attribute classifier
    , attrPos          :: C.AttrPos C.AttrName   -- Positional attribute
    , attrPosNames     :: [C.AttrName]           -- Positional names
    , attrBranchNames  :: [C.AttrName]           -- Branch names
    }

ropAttrCons :: C.AttrPos C.AttrName -> [C.AttrName] -> RopAttr
ropAttrCons pos branchNames = ropAttr where
    ropAttr      = RopAttr posSorter classify pos posNames branchNames
    posSorter    = C.ropAttrPos pos
    posNames     = C.attrTypeNames pos
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
--   >>> Right . hyphenAssc =<< B.tt "a b -x /c 'd -y e"
--   [ ("@trunk", [TreeL (TText 1 0 "a"), TreeL (TText 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TText 9 1 "d")])
--   , ("-y", [TreeL (TText 14 0 "e")]) ]

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type AttrSortPara = [B.TTree] -> B.Ab AttrPara

type AttrPara = B.ParaBody C.AttrName B.TTree

attrSort :: RopAttr -> AttrSortPara
attrSort def = attrBranch B.>=> attrSortPos def

attrBranch :: AttrSortPara
attrBranch trees =
    do let p   = B.para maybeSingleHyphen trees
           p2  = B.paraNameAdd "@trunk" (B.paraPos p) p
           dup = B.paraMultipleNames p2
       B.when (B.notNull dup) $ Msg.dupAttr dup
       Right $ B.paraNameMapKeys C.AttrNormal p2

maybeSingleHyphen :: B.TTreeTo (Maybe String)
maybeSingleHyphen (B.TextLeafRaw _ n@('-' : _))  = Just n
maybeSingleHyphen _                              = Nothing

attrSortPos :: RopAttr -> B.AbMap AttrPara
attrSortPos (RopAttr sorter classify _ pos named) p =
    do let noPos      = null $ B.paraPos p
           nameList   = map fst $ B.paraNameList p
           overlapped = pos `B.overlap` nameList

       p2            <- case noPos && overlapped of
                          True  -> Right p
                          False -> B.paraPosName sorter p

       let p3         = B.paraNameMapKeys classify p2
           attr       = attrList p3

       attrCheck pos named attr
       Right p3

attrCheck :: [C.AttrName] -> [C.AttrName] -> [C.AttrTree] -> B.Ab ()
attrCheck posNames branchNames attr =
    case t (map fst attr) B.\\ textAll of
      []    -> Right ()
      u : _ -> Msg.unexpAttr $ "Unknown " ++ u
    where
      textAll = "@trunk" : t posNames ++ t branchNames
      t       = map C.attrNameText

attrList :: B.ParaBody n a -> [(n, [a])]
attrList = map (B.mapSnd concat) . B.paraNameList

