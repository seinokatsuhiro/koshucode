{-# OPTIONS_GHC -Wall #-}

-- | Operand sorters for relmap operators.

module Koshucode.Baala.Core.Relmap.Operand
( -- * Data type
  Rod,
  Rody,
  RodSorter,
  RodSpec,

  -- * Branch sorter
  rod,
  rodBranch,
  rodSorter,

  -- * Trunk sorter
  sortNone,
  sortEnum,
  sortList,
  sortOne,
  sortTwo,
  sortThree,
  sortFour,
  sortOneList,
  sortOneOpt,
  -- $TrunkSorter
) where

import qualified Data.List                    as List
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Message



-- ----------------------  Data type

-- | Relmap operand as association list.
type Rod = [B.Named [B.TokenTree]]

-- | Association of operand use and something.
--   Operand use is represented as pair of operator name and operand.
type Rody a = ((String, Rod), a)

-- | Sorter for operand of relmap operator.
--   Sorters docompose operand trees,
--   and give a name to suboperand.
type RodSorter = [B.TokenTree] -> B.Ab Rod

-- | Operand sorter for relmap operator.
--   It consists of trunk sorter, trunk names, and branch names.
type RodSpec =
    ( B.AbMap Rod  -- Trunk sorter
    , [String]     -- Trunk names
    , [String]     -- Branch names
    )


-- ----------------------  Branch sorter

-- | Split operand into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> rod $ B.tt "a b -x /c 'd -y e"
--   [ ("@trunk", [TreeL (TWord 1 0 "a"), TreeL (TWord 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TWord 9 1 "d")])
--   , ("-y", [TreeL (TWord 14 0 "e")]) ]
--
rod :: [B.TokenTree] -> Rod
rod = B.assocBy branchName "@trunk" where
    branchName (B.TreeL (B.TWord _ 0 n@('-' : _))) = Just n
    branchName _ = Nothing

rodSorter :: RodSpec -> RodSorter
rodSorter spec = rodBranch B.>=> rodTrunk spec

rodBranch :: RodSorter
rodBranch trees = sorted where
    a :: Rod
    a = rod trees

    dup :: [String]
    dup = map fst $ B.assocMore $ B.assocGather a

    sorted :: B.Ab Rod
    sorted | null dup  = Right a
           | otherwise = Message.unexpOperand $ "Duplicate " ++ unwords dup

rodTrunk :: RodSpec -> B.AbMap Rod
rodTrunk (trunkSorter, trunkNames, branchNames) assoc = sorted where
    alls, given, unk, wrap :: [String]
    alls  = "@trunk" : trunkNames ++ branchNames
    given = map fst assoc
    unk   = given  List.\\  alls
    wrap  = given `List.intersect` trunkNames

    sorted :: B.Ab Rod
    sorted | B.notNull unk  = Message.unexpOperand $ "Unknown " ++ unwords unk
           | B.notNull wrap = Right assoc
           | otherwise      = trunkSorter assoc



-- ----------------------  Trunk sorters

-- $TrunkSorter
--
--  /Examples/
--
--  One required operand and no options.
--
--    > sortOne "-term" []
--
--  Two required operands and no options.
--
--    > sortTwo "-name" "-relmap" []
--
--  Any number of operands and no options.
--
--    > sortList "-term" []
--
--  One and any number of operands.
--
--    > sortOneList "-pattern" "-term" []

-- | Operand sorter for no-element trunk.
sortNone :: [String] -> RodSpec
sortNone ns = (Right, [], ns)

-- | Operand sorter for enumerating trunk.
sortEnum :: [String] -> [String] -> RodSpec
sortEnum ks ns = (by f, ks, ns) where
    f xs  = Right $ zip names $ map B.singleton xs
    names = map (('-' :) . show) [1 :: Int ..]

-- | Operand sorter for multiple-element trunk.
sortList :: String -> [String] -> RodSpec
sortList a ns = (by f, [a], ns) where
    f xs = Right [ (a, xs) ]

-- | Operand sorter for one-element trunk.
sortOne :: String -> [String] -> RodSpec
sortOne a ns = (by f, [a], ns) where
    f [x] = Right [ (a, [x]) ]
    f _   = Message.unexpOperand "Require one operand"

-- | Operand sorter for two-element trunk.
sortTwo :: String -> String -> [String] -> RodSpec
sortTwo a b ns = (by f, [a,b], ns) where
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpOperand "Require two operands"

sortThree :: String -> String -> String -> [String] -> RodSpec
sortThree a b c ns = (by f, [a,b,c], ns) where
    f [x,y,z] = Right [ (a, [x]), (b, [y]), (c, [z]) ]
    f _       = Message.unexpOperand "Require three operands"

sortFour :: String -> String -> String -> String -> [String] -> RodSpec
sortFour a b c d ns = (by f, [a,b,c,d], ns) where
    f [x1,x2,x3,x4] = Right [ (a, [x1]), (b, [x2]), (c, [x3]), (d, [x4]) ]
    f _             = Message.unexpOperand "Require four operands"

-- | Operand sorter for one-and-multiple-element trunk.
sortOneList :: String -> String -> [String] -> RodSpec
sortOneList a b ns = (by f, [a,b], ns) where
    f (x:xs) = Right [ (a, [x]), (b, xs) ]
    f _      = Message.unexpOperand "Require operands"

sortOneOpt :: String -> String -> [String] -> RodSpec
sortOneOpt a b ns = (by f, [a,b], ns) where
    f [x]   = Right [ (a, [x]), (b, []) ]
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpOperand "Require two operands"

-- | Give a name to unnamed operand.
by :: RodSorter -> B.AbMap Rod
by f xs = case lookup "@trunk" xs of
            Just x  -> Right . (++ xs) =<< f x
            Nothing -> Right xs

