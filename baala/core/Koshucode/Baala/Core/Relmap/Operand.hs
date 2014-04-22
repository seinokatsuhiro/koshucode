{-# OPTIONS_GHC -Wall #-}

-- | Operand sorters for relmap operators.

module Koshucode.Baala.Core.Relmap.Operand
( -- * Full sorter
  RopFullSorter,
  ropFullSorter,
  ropBaseSorter,

  -- * Branch sorter
  RopOperandAssoc,
  ropOperandAssoc,

  -- * Trunk sorter
  RopOperandSorter,
  RopTrunkSorter,
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



-- ----------------------  Full sorter

-- | Sorter for operand of relmap operator.
--   Sorters docompose operand trees,
--   and give a name to suboperand.
type RopFullSorter = [B.TokenTree] -> B.Ab RopOperandAssoc

ropFullSorter :: RopOperandSorter -> RopFullSorter
ropFullSorter sorter = ropUserSorter sorter B.<=< ropBaseSorter

ropBaseSorter :: RopFullSorter
ropBaseSorter trees = sorted where
    assoc :: RopOperandAssoc
    assoc = ropOperandAssoc trees

    dup :: [String]
    dup = map fst $ B.assocMore $ B.assocGather assoc

    sorted :: B.Ab RopOperandAssoc
    sorted | null dup   = Right assoc
           | otherwise  = Message.unexpOperand $ "Duplicate " ++ unwords dup

ropUserSorter :: RopOperandSorter -> B.AbMap RopOperandAssoc
ropUserSorter (trunkSorter, trunkNames, branchNames) assoc = sorted where
    alls, given, unk, wrap :: [String]
    alls  = "" : trunkNames ++ branchNames
    given = map fst assoc
    unk   = given  List.\\  alls
    wrap  = given `List.intersect` trunkNames

    exists = not . null

    sorted :: B.Ab RopOperandAssoc
    sorted | exists unk  = Message.unexpOperand $ "Unknown " ++ unwords unk
           | exists wrap = Right assoc
           | otherwise   = trunkSorter assoc



-- ----------------------  Branch sorter

-- | Relmap operand as association list.
type RopOperandAssoc = [B.Named [B.TokenTree]]

-- | Split operand into named group.
--   Non quoted words beginning with hyphen, e.g., @-x@,
--   are name of group.
--
--   >>> ropOperandAssoc $ B.tt "a b -x /c 'd -y e"
--   [ ("",   [TreeL (TWord 1 0 "a"), TreeL (TWord 3 0 "b")])
--   , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TWord 9 1 "d")])
--   , ("-y", [TreeL (TWord 14 0 "e")]) ]
--
ropOperandAssoc :: [B.TokenTree] -> RopOperandAssoc
ropOperandAssoc = B.assocBy maybeBranch "" where
    maybeBranch (B.TreeL (B.TWord _ 0 n@('-' : _))) = Just n
    maybeBranch _ = Nothing



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

-- | Operand sorter for relmap operator.
--   It consists of trunk sorter, trunk names, and branch names.
--   Trunk part is unnamed operand.
--   Branch part is named operand.
type RopOperandSorter =
    ( RopTrunkSorter  -- Trunk sorter
    , [String]        -- Trunk names
    , [String]        -- Branch names
    )

-- | Mapping from basically sorted operand
--   to fully sorted operand.
type RopTrunkSorter =  B.AbMap RopOperandAssoc

-- | Operand sorter for no-element trunk.
sortNone :: [String] -> RopOperandSorter
sortNone ns = (Right, [], ns)

-- | Operand sorter for enumerating trunk.
sortEnum :: [String] -> [String] -> RopOperandSorter
sortEnum ks ns = (by f, ks, ns) where
    f xs  = Right $ zip names $ map B.singleton xs
    names = map (('-' :) . show) [1 :: Int ..]

-- | Operand sorter for multiple-element trunk.
sortList :: String -> [String] -> RopOperandSorter
sortList a ns = (by f, [a], ns) where
    f xs = Right [ (a, xs) ]

-- | Operand sorter for one-element trunk.
sortOne :: String -> [String] -> RopOperandSorter
sortOne a ns = (by f, [a], ns) where
    f [x] = Right [ (a, [x]) ]
    f _   = Message.unexpOperand "Require one operand"

-- | Operand sorter for two-element trunk.
sortTwo :: String -> String -> [String] -> RopOperandSorter
sortTwo a b ns = (by f, [a,b], ns) where
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpOperand "Require two operands"

sortThree :: String -> String -> String -> [String] -> RopOperandSorter
sortThree a b c ns = (by f, [a,b,c], ns) where
    f [x,y,z] = Right [ (a, [x]), (b, [y]), (c, [z]) ]
    f _       = Message.unexpOperand "Require three operands"

sortFour :: String -> String -> String -> String -> [String] -> RopOperandSorter
sortFour a b c d ns = (by f, [a,b,c,d], ns) where
    f [x1,x2,x3,x4] = Right [ (a, [x1]), (b, [x2]), (c, [x3]), (d, [x4]) ]
    f _             = Message.unexpOperand "Require four operands"

-- | Operand sorter for one-and-multiple-element trunk.
sortOneList :: String -> String -> [String] -> RopOperandSorter
sortOneList a b ns = (by f, [a,b], ns) where
    f (x:xs) = Right [ (a, [x]), (b, xs) ]
    f _      = Message.unexpOperand "Require operands"

sortOneOpt :: String -> String -> [String] -> RopOperandSorter
sortOneOpt a b ns = (by f, [a,b], ns) where
    f [x]   = Right [ (a, [x]), (b, []) ]
    f [x,y] = Right [ (a, [x]), (b, [y]) ]
    f _     = Message.unexpOperand "Require two operands"

-- | Give a name to unnamed operand.
by :: RopFullSorter -> RopTrunkSorter
by f xs = case lookup "" xs of
            Just x  -> Right . (++ xs) =<< f x
            Nothing -> Right xs

