{-# OPTIONS_GHC -Wall #-}

-- | Attribute sorters.

module Koshucode.Baala.Core.Lexmap.Sorter
( -- * Trunk sorter
  roaNone, roaEnum,
  roaList, roaOneList, roaOneOpt,
  roaOne, roaTwo, roaThree, roaFour,
  -- $TrunkSorter
) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute as C
import qualified Koshucode.Baala.Core.Message          as Message


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
name :: C.RoaSorter -> B.AbMap C.Roa
name f roa = case lookup C.attrNameTrunk roa of
               Just xs -> Right . (++ roa) =<< f xs
               Nothing -> Right roa

-- | Attribute sorter for no-attribute trunk.
roaNone :: [C.AttrName] -> C.RoaSpec
roaNone ns = (name f, [], ns) where
    f []            = Right []
    f _             = Message.unexpAttr "Attributes not required"

-- | Attribute sorter for enumerating trunk,
--   i.e., @-1@, @-2@, ...
roaEnum :: [C.AttrName] -> [C.AttrName] -> C.RoaSpec
roaEnum ks ns = (name f, ks, ns) where
    f = Right . zip enumAttr . map B.li1

enumAttr :: [String]
enumAttr = map (('-' :) . show) [1 :: Int ..]

-- | Attribute sorter for multiple-attribute trunk.
roaList :: C.AttrName -> [C.AttrName] -> C.RoaSpec
roaList a ns        = (name f, [a], ns) where
    f xs            = Right [ (a, xs) ]

-- | Attribute sorter for one-and-multiple-attribute trunk.
roaOneList :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOneList a b ns = (name f, [a,b], ns) where
    f (a':b')       = Right [ (a, B.li1 a'), (b, b') ]
    f _             = Message.unexpAttr "Require attributes"

roaOneOpt :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOneOpt a b ns    = (name f, [a,b], ns) where
    f [a']          = Right [ (a, B.li1 a'), (b, []) ]
    f [a',b']       = Right [ (a, B.li1 a'), (b, B.li1 b') ]
    f _             = Message.unexpAttr "Require one or two attributes"

-- | Attribute sorter for one-attribute trunk.
roaOne :: C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOne a ns         = (name f, [a], ns) where
    f [a']          = Right [ (a, B.li1 a') ]
    f _             = Message.unexpAttr "Require one attribute"

-- | Attribute sorter for two-attribute trunk.
roaTwo :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaTwo a b ns       = (name f, [a,b], ns) where
    f [a',b']       = Right [ (a, B.li1 a'), (b, B.li1 b') ]
    f _             = Message.unexpAttr "Require two attributes"

-- | Attribute sorter for three-attribute trunk.
roaThree :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaThree a b c ns   = (name f, [a,b,c], ns) where
    f [a',b',c']    = Right [ (a, B.li1 a'), (b, B.li1 b'), (c, B.li1 c') ]
    f _             = Message.unexpAttr "Require three attributes"

-- | Attribute sorter for four-attribute trunk.
roaFour :: C.AttrName -> C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaFour a b c d ns  = (name f, [a,b,c,d], ns) where
    f [a',b',c',d'] = Right [ (a, B.li1 a'), (b, B.li1 b'), (c, B.li1 c'), (d, B.li1 d') ]
    f _             = Message.unexpAttr "Require four attributes"

