{-# OPTIONS_GHC -Wall #-}

-- | Attribute sorters.

module Koshucode.Baala.Core.Lexmap.Sorter
( -- * Trunk sorter
  roaNone, roaEnum, roaList,
  roaOne, roaTwo, roaThree, roaFour,
  roaOneList, roaOneOpt,
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

-- | Attribute sorter for no-element trunk.
roaNone :: [C.AttrName] -> C.RoaSpec
roaNone ns = (Right, [], ns)

-- | Attribute sorter for enumerating trunk.
roaEnum :: [C.AttrName] -> [C.AttrName] -> C.RoaSpec
roaEnum ks ns = (by f, ks, ns) where
    f xs  = Right $ zip names $ map B.singleton xs
    names = map (('-' :) . show) [1 :: Int ..]

-- | Attribute sorter for multiple-element trunk.
roaList :: C.AttrName -> [C.AttrName] -> C.RoaSpec
roaList a ns = (by f, [a], ns) where
    f xs = Right [ (a, xs) ]

-- | Attribute sorter for one-element trunk.
roaOne :: C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOne a ns = (by f, [a], ns) where
    f [x] = Right [ (a, [x]) ]
    f _   = Message.unexpAttr "Require one attribute"

-- | Attribute sorter for two-element trunk.
roaTwo :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaTwo a b ns = (by f, [a,b], ns) where
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpAttr "Require two attributes"

roaThree :: C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaThree a b c ns = (by f, [a,b,c], ns) where
    f [x,y,z] = Right [ (a, [x]), (b, [y]), (c, [z]) ]
    f _       = Message.unexpAttr "Require three attributes"

roaFour :: C.AttrName -> C.AttrName -> C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaFour a b c d ns = (by f, [a,b,c,d], ns) where
    f [x1,x2,x3,x4] = Right [ (a, [x1]), (b, [x2]), (c, [x3]), (d, [x4]) ]
    f _             = Message.unexpAttr "Require four attributes"

-- | Attribute sorter for one-and-multiple-element trunk.
roaOneList :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOneList a b ns = (by f, [a,b], ns) where
    f (x:xs) = Right [ (a, [x]), (b, xs) ]
    f _      = Message.unexpAttr "Require attributes"

roaOneOpt :: C.AttrName -> C.AttrName -> [C.AttrName] -> C.RoaSpec
roaOneOpt a b ns = (by f, [a,b], ns) where
    f [x]   = Right [ (a, [x]), (b, []) ]
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpAttr "Require two attributes"

-- | Give a name to unnamed attribute.
by :: C.RoaSorter -> B.AbMap C.Roa
by f roa = case lookup C.attrNameTrunk roa of
            Just x  -> Right . (++ roa) =<< f x
            Nothing -> Right roa
