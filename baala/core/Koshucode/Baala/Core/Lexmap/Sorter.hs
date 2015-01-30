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

-- | Give a name to unnamed attribute.
name :: C.AttrSort -> B.AbMap [C.AttrTree]
name f roa = case lookup C.attrNameTrunk roa of
               Just xs -> Right . (++ roa) =<< f xs
               Nothing -> Right roa

spec :: B.AbMap [C.AttrTree] -> [C.AttrName] -> [C.AttrName] -> C.AttrDefine
spec trunkSorter trunkNames branchNames =
    C.AttrDefine trunkSorter classify trunkNames branchNames where
        classify = attrClassify trunkNames branchNames

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

posName :: B.Ab [(n, [a])] -> ([a] -> Maybe [(n, [a])]) -> [a] -> B.Ab [(n, [a])]
posName msg p xs =
    case p xs of
      Just attr -> Right attr
      Nothing   -> msg

-- | Attribute sorter for no-attribute trunk.
roaNone :: [C.AttrName] -> C.AttrDefine
roaNone = spec (name $ posName Msg.unexpAttr0 posName0) []

-- | Attribute sorter for enumerating trunk,
--   i.e., @-1@, @-2@, ...
roaEnum :: [C.AttrName] -> [C.AttrName] -> C.AttrDefine
roaEnum ks = spec (name $ Right . zip enumAttr . map B.li1) ks

enumAttr :: [C.AttrName]
enumAttr = map (C.AttrNameNormal . ('-' :) . show) [1 :: Int ..]

-- | Attribute sorter for multiple-attribute trunk.
roaList :: C.AttrName -> [C.AttrName] -> C.AttrDefine
roaList a           = spec (name $ posName (Msg.adlib "") $ posNameV a) [a] where

-- | Attribute sorter for one-and-multiple-attribute trunk.
roaOneList :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOneList a b      = spec (name $ posName Msg.unexpAttr1V $ posName1V a b) [a,b]

roaOneOpt :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOneOpt a b       = spec (name $ posName Msg.unexpAttr1Q $ posName1Q a b) [a,b]

-- | Attribute sorter for one-attribute trunk.
roaOne :: C.AttrName -> [C.AttrName] -> C.AttrDefine
roaOne a            = spec (name $ posName Msg.unexpAttr1 $ posName1 a) [a]

-- | Attribute sorter for two-attribute trunk.
roaTwo :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTwo a b          = spec (name $ posName Msg.unexpAttr2 $ posName2 a b) [a,b]

-- | Attribute sorter for three-attribute trunk.
roaThree :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaThree a b c      = spec (name $ posName Msg.unexpAttr3 $ posName3 a b c) [a,b,c]

-- | Attribute sorter for four-attribute trunk.
roaFour :: C.AttrName -> C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaFour a b c d     = spec (name $ posName Msg.unexpAttr4 $ posName4 a b c d) [a,b,c,d] where

roaTermsOne :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTermsOne a b = spec (name $ posName Msg.unexpAttrT1 $ posNameT1 a b) [a,b] where

roaTermsTwo :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.AttrDefine
roaTermsTwo a b c = spec (name $ posName Msg.unexpAttrT2 $ posNameT2 a b c) [a,b,c]


-- ----------------------  Positional name

posName0 :: [a] -> Maybe [(n, [a])]
posName0        = f where
    f []        = Just []
    f _         = Nothing

posName1 :: n -> [a] -> Maybe [(n, [a])]
posName1 a      = f where
    f [aa]      = Just [(a,[aa])]
    f _         = Nothing

posName2 :: n -> n -> [a] -> Maybe [(n, [a])]
posName2 a b    = f where
    f [aa,bb]   = Just [(a,[aa]), (b,[bb])]
    f _         = Nothing

posName3 :: n -> n -> n -> [a] -> Maybe [(n, [a])]
posName3 a b c    = f where
    f [aa,bb,cc]  = Just [(a,[aa]), (b,[bb]), (c,[cc])]
    f _           = Nothing

posName4 :: n -> n -> n -> n -> [a] -> Maybe [(n, [a])]
posName4 a b c d     = f where
    f [aa,bb,cc,dd]  = Just [(a,[aa]), (b,[bb]), (c,[cc]), (d,[dd])]
    f _              = Nothing

posNameV :: n -> [a] -> Maybe [(n, [a])]
posNameV a aa     = Just [(a,aa)]

posName1V :: n -> n -> [a] -> Maybe [(n, [a])]
posName1V a b     = f where
    f (aa:bb)     = Just [(a,[aa]), (b,bb)]
    f _           = Nothing

posName1Q :: n -> n -> [a] -> Maybe [(n, [a])]
posName1Q a b     = f where
    f [aa]        = Just [(a,[aa]), (b,[])]
    f [aa,bb]     = Just [(a,[aa]), (b,[bb])]
    f _           = Nothing

posNameT1 :: n -> n -> [B.TTree] -> Maybe [(n, [B.TTree])]
posNameT1 a b = f where
    f xs = case span isTermLeaf xs of
             (aa, [bb]) -> Just [ (a,aa), (b,[bb]) ]
             _          -> Nothing

posNameT2 :: n -> n -> n -> [B.TTree] -> Maybe [(n, [B.TTree])]
posNameT2 a b c = f where
    f xs = case span isTermLeaf xs of
             (aa, [bb, cc]) -> Just [ (a,aa), (b,[bb]), (c,[cc]) ]
             _              -> Nothing

isTermLeaf :: B.TTree -> Bool
isTermLeaf (B.TreeL token) = B.isTermToken token
isTermLeaf _               = False
