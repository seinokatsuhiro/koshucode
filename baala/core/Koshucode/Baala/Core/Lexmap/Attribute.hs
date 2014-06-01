{-# OPTIONS_GHC -Wall #-}

-- | Relmap operator attributes.

module Koshucode.Baala.Core.Lexmap.Attribute
( -- * Data type
  Roa,
  Roal,
  RoaSorter,
  RoaSpec,

  -- * Branch sorter
  roa,
  roaBranch,
  roaSorter,

  -- * Trunk sorter
  roaNone,
  roaEnum,
  roaList,
  roaOne,
  roaTwo,
  roaThree,
  roaFour,
  roaOneList,
  roaOneOpt,
  -- $TrunkSorter
) where

import qualified Data.List                    as List
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Message



-- ----------------------  Data type

-- | Relmap operation attributes as association list.
type Roa = [B.NamedTrees]

-- | Association of operator use and something.
--   Operator use is represented as pair of operator name and attributes.
type Roal a = ((String, Roa), a)

-- | Sorter for attribute of relmap operator.
--   Sorters docompose attribute trees,
--   and give a name to subattribute.
type RoaSorter = [B.TokenTree] -> B.Ab Roa

-- | Attribute sorter for relmap operator.
--   It consists of trunk sorter, trunk names, and branch names.
type RoaSpec =
    ( B.AbMap Roa  -- Trunk sorter
    , [String]     -- Trunk names
    , [String]     -- Branch names
    )


-- ----------------------  Branch sorter

-- | Split attribute into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> roa $ B.tt "a b -x /c 'd -y e"
--   [ ("@trunk", [TreeL (TText 1 0 "a"), TreeL (TText 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TText 9 1 "d")])
--   , ("-y", [TreeL (TText 14 0 "e")]) ]
--
roa :: [B.TokenTree] -> Roa
roa = B.assocBy branchName "@trunk" where
    branchName (B.TreeL (B.TText _ 0 n@('-' : _))) = Just n
    branchName _ = Nothing

roaSorter :: RoaSpec -> RoaSorter
roaSorter spec = roaBranch B.>=> roaTrunk spec

roaBranch :: RoaSorter
roaBranch trees = sorted where
    a :: Roa
    a = roa trees

    dup :: [String]
    dup = map fst $ B.assocMore $ B.assocGather a

    sorted :: B.Ab Roa
    sorted | null dup  = Right a
           | otherwise = Message.unexpAttr $ "Duplicate " ++ unwords dup

roaTrunk :: RoaSpec -> B.AbMap Roa
roaTrunk (trunkSorter, trunkNames, branchNames) assoc = sorted where
    alls, given, unk, wrap :: [String]
    alls  = "@trunk" : trunkNames ++ branchNames
    given = map fst assoc
    unk   = given  List.\\  alls
    wrap  = given `List.intersect` trunkNames

    sorted :: B.Ab Roa
    sorted | B.notNull unk  = Message.unexpAttr $ "Unknown " ++ unwords unk
           | B.notNull wrap = Right assoc
           | otherwise      = trunkSorter assoc



-- ----------------------  Trunk sorters

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
roaNone :: [String] -> RoaSpec
roaNone ns = (Right, [], ns)

-- | Attribute sorter for enumerating trunk.
roaEnum :: [String] -> [String] -> RoaSpec
roaEnum ks ns = (by f, ks, ns) where
    f xs  = Right $ zip names $ map B.singleton xs
    names = map (('-' :) . show) [1 :: Int ..]

-- | Attribute sorter for multiple-element trunk.
roaList :: String -> [String] -> RoaSpec
roaList a ns = (by f, [a], ns) where
    f xs = Right [ (a, xs) ]

-- | Attribute sorter for one-element trunk.
roaOne :: String -> [String] -> RoaSpec
roaOne a ns = (by f, [a], ns) where
    f [x] = Right [ (a, [x]) ]
    f _   = Message.unexpAttr "Require one attribute"

-- | Attribute sorter for two-element trunk.
roaTwo :: String -> String -> [String] -> RoaSpec
roaTwo a b ns = (by f, [a,b], ns) where
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpAttr "Require two attributes"

roaThree :: String -> String -> String -> [String] -> RoaSpec
roaThree a b c ns = (by f, [a,b,c], ns) where
    f [x,y,z] = Right [ (a, [x]), (b, [y]), (c, [z]) ]
    f _       = Message.unexpAttr "Require three attributes"

roaFour :: String -> String -> String -> String -> [String] -> RoaSpec
roaFour a b c d ns = (by f, [a,b,c,d], ns) where
    f [x1,x2,x3,x4] = Right [ (a, [x1]), (b, [x2]), (c, [x3]), (d, [x4]) ]
    f _             = Message.unexpAttr "Require four attributes"

-- | Attribute sorter for one-and-multiple-element trunk.
roaOneList :: String -> String -> [String] -> RoaSpec
roaOneList a b ns = (by f, [a,b], ns) where
    f (x:xs) = Right [ (a, [x]), (b, xs) ]
    f _      = Message.unexpAttr "Require attributes"

roaOneOpt :: String -> String -> [String] -> RoaSpec
roaOneOpt a b ns = (by f, [a,b], ns) where
    f [x]   = Right [ (a, [x]), (b, []) ]
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpAttr "Require two attributes"

-- | Give a name to unnamed attribute.
by :: RoaSorter -> B.AbMap Roa
by f xs = case lookup "@trunk" xs of
            Just x  -> Right . (++ xs) =<< f x
            Nothing -> Right xs

