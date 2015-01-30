{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute sorters.

module Koshucode.Baala.Core.Lexmap.Sorter
  ( -- * Trunk sorter
    roaNone, roaEnum,
    roaList, roaOneList, roaOneOpt,
    roaOne, roaTwo, roaThree, roaFour,
    roaTermsOne, roaTermsTwo,
    -- $TrunkSorter
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- $TrunkSorter
--
--  /Examples/
--
--  One required attribute and no options.
--
--    > roaOne "-term" []
--
--  Two required attributes and no options.
--
--    > roaTwo "-name" "-relmap" []
--
--  Any number of attributes and no options.
--
--    > roaList "-term" []
--
--  One and any number of attributes.
--
--    > roaOneList "-pattern" "-term" []

def :: C.AttrSort -> [C.AttrName] -> [C.AttrName] -> C.AttrDefine
def trunkSorter trunkNames branchNames =
    C.AttrDefine (name trunkSorter) classify trunkNames branchNames where
        classify = attrClassify trunkNames branchNames

-- | Give a name to unnamed attribute.
name :: C.AttrSort -> B.AbMap [C.AttrTree]
name f roa = case lookup C.attrNameTrunk roa of
               Just xs -> Right . (++ roa) =<< f xs
               Nothing -> Right roa

attrClassify :: [C.AttrName] -> [C.AttrName] -> B.AbMap [C.AttrTree]
attrClassify trunkNames branchNames roa = roa2 where
    roa2     :: B.Ab [C.AttrTree]
    roa2     = B.sequenceFst $ B.mapFstTo relmap roa

    relmap :: B.AbMap C.AttrName
    relmap n = let nam = C.attrNameText n
               in case lookup nam pairs of
                 Just k  -> Right k
                 Nothing -> Msg.unexpAttr $ "Unknown " ++ nam

    pairs    :: [B.Named C.AttrName]
    pairs    = map pair alls
    pair n   = (C.attrNameText n, n)
    alls     = C.attrNameTrunk : trunkNames ++ branchNames

pn :: B.Ab [(n, [a])] -> ([a] -> Maybe [(n, [a])]) -> [a] -> B.Ab [(n, [a])]
pn msg p xs =
    case p xs of
      Just attr -> Right attr
      Nothing   -> msg

enumAttr :: [C.AttrName]
enumAttr = map (C.AttrNameNormal . ('-' :) . show) [1 :: Int ..]

-- | Attribute sorter for enumerating trunk,
--   i.e., @-1@, @-2@, ...
roaEnum :: [C.AttrName] -> [C.AttrName] -> C.AttrDefine
roaEnum ks          = def (Right . zip enumAttr . map B.li1) ks

-- | Attribute sorter for no-attribute trunk.
roaNone :: [C.AttrName] -> C.AttrDefine
roaNone             = def (pn Msg.unexpAttr0 posName0) []

-- | Attribute sorter for multiple-attribute trunk.
roaList :: C.AttrName -> [C.AttrName] -> C.AttrDefine
roaList a           = def (pn (Msg.bug "list") $ posNameV a) [a]

-- | Attribute sorter for one-and-multiple-attribute trunk.
roaOneList :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOneList a b      = def (pn Msg.unexpAttr1V $ posName1V a b) [a,b]

roaOneOpt :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOneOpt a b       = def (pn Msg.unexpAttr1Q $ posName1Q a b) [a,b]

-- | Attribute sorter for one-attribute trunk.
roaOne :: C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOne a            = def (pn Msg.unexpAttr1 $ posName1 a) [a]

-- | Attribute sorter for two-attribute trunk.
roaTwo :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTwo a b          = def (pn Msg.unexpAttr2 $ posName2 a b) [a,b]

-- | Attribute sorter for three-attribute trunk.
roaThree :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaThree a b c      = def (pn Msg.unexpAttr3 $ posName3 a b c) [a,b,c]

-- | Attribute sorter for four-attribute trunk.
roaFour :: C.AttrName -> C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaFour a b c d     = def (pn Msg.unexpAttr4 $ posName4 a b c d) [a,b,c,d]

roaTermsOne :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTermsOne a b     = def (pn Msg.unexpAttrT1 $ posNameT1 a b) [a,b]

roaTermsTwo :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTermsTwo a b c   = def (pn Msg.unexpAttrT2 $ posNameT2 a b c) [a,b,c]


-- ----------------------  Positional name

posName0 :: [a] -> Maybe [(n, [a])]
posName0 []                     = Just []
posName0 _                      = Nothing

posName1 :: n -> [a] -> Maybe [(n, [a])]
posName1 a [aa]                 = Just [(a,[aa])]
posName1 _ _                    = Nothing

posName2 :: n -> n -> [a] -> Maybe [(n, [a])]
posName2 a b [aa,bb]            = Just [(a,[aa]), (b,[bb])]
posName2 _ _ _                  = Nothing

posName3 :: n -> n -> n -> [a] -> Maybe [(n, [a])]
posName3 a b c [aa,bb,cc]       = Just [(a,[aa]), (b,[bb]), (c,[cc])]
posName3 _ _ _ _                = Nothing

posName4 :: n -> n -> n -> n -> [a] -> Maybe [(n, [a])]
posName4 a b c d [aa,bb,cc,dd]  = Just [(a,[aa]), (b,[bb]), (c,[cc]), (d,[dd])]
posName4 _ _ _ _ _              = Nothing

posNameV :: n -> [a] -> Maybe [(n, [a])]
posNameV a aa                   = Just [(a,aa)]

posName1V :: n -> n -> [a] -> Maybe [(n, [a])]
posName1V a b (aa:bb)           = Just [(a,[aa]), (b,bb)]
posName1V _ _ _                 = Nothing

posName1Q :: n -> n -> [a] -> Maybe [(n, [a])]
posName1Q a b [aa]              = Just [(a,[aa]), (b,[])]
posName1Q a b [aa,bb]           = Just [(a,[aa]), (b,[bb])]
posName1Q _ _ _                 = Nothing

posNameT1 :: n -> n -> [B.TTree] -> Maybe [(n, [B.TTree])]
posNameT1 a b xs   = case span isTermLeaf xs of
                       (aa, [bb])  -> Just [ (a,aa), (b,[bb]) ]
                       _           -> Nothing

posNameT2 :: n -> n -> n -> [B.TTree] -> Maybe [(n, [B.TTree])]
posNameT2 a b c xs = case span isTermLeaf xs of
                       (aa, [bb,cc])  -> Just [ (a,aa), (b,[bb]), (c,[cc]) ]
                       _              -> Nothing

isTermLeaf :: B.TTree -> Bool
isTermLeaf (B.TreeL token) = B.isTermToken token
isTermLeaf _               = False
