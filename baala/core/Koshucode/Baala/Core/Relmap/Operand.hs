{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Operand
( -- * Datatype
  RopOperand,
  RopSorter,
  RopFullSorter,
  RopAssoc,
  sortOperand,
  sortUpOperand,

  -- * Sorter
  operandNone,
  operandEnum,
  operandList,
  operandOne,
  operandTwo,
  operandOneList,
) where

import qualified Koshucode.Baala.Base as B

type RopOperand = (RopSorter, [String], [String])

{-| Sorter for operand of relational operator.
    This soters docompose operand trees,
    and give a name to suboperand. -}
type RopFullSorter
    =  [B.TokenTree]   -- ^ Unsorted operand
    -> B.Ab RopAssoc   -- ^ Fully sorted operand

type RopSorter
    =  RopAssoc        -- ^ Basically sorted operand
    -> B.Ab RopAssoc   -- ^ Fully sorted operand

type RopAssoc = [B.Named [B.TokenTree]]

{-| Split operand into named group.
    Non quoted words beginning with hyphen, e.g., @-x@,
    are name of group.
  
    >>> sortOperand $ B.tt "a b -x /c 'd -y e"
    [ ("",   [TreeL (TWord 1 0 "a"), TreeL (TWord 3 0 "b")])
    , ("-x", [TreeL (TTerm 7 ["/c"]), TreeL (TWord 9 1 "d")])
    , ("-y", [TreeL (TWord 14 0 "e")]) ]
  -}

sortOperand :: [B.TokenTree] -> RopAssoc
sortOperand = B.assocBy maybeBranch "" where
    maybeBranch (B.TreeL (B.TWord _ 0 n@('-' : _))) = Just n
    maybeBranch _ = Nothing

sortUpOperand :: [B.TokenTree] -> [B.Named (B.OnceMore [B.TokenTree])]
sortUpOperand = B.assocGather . sortOperand

-- ----------------------

{-| No-element trunk.

    >>> operandNone [] -}
operandNone :: [String] -> RopOperand
operandNone ns = (Right, [], ns)

{-| Enumerating trunk.

    >>> operandEnum ["-1", "-2"] [] -}
operandEnum :: [String] -> [String] -> RopOperand
operandEnum ks ns = (trunkBy f, ks, ns) where
    f xs          = Right $ zip names $ map B.singleton xs
    names         = map (('-' :) . show) [1 :: Int ..]

{-| Multiple-element trunk.

    >>> operandList "-term" [] -}
operandList :: String -> [String] -> RopOperand
operandList a ns = (trunkBy f, [a], ns) where
    f xs         = Right [ (a, xs) ]

{-| One-element trunk.

    >>> operandOne "-relmap" [] -}
operandOne :: String -> [String] -> RopOperand
operandOne a ns = (trunkBy f, [a], ns) where
    f [x]       = Right [ (a, [x]) ]
    f _         = Left $ B.abortMalformedOperand "unary"

{-| Two-element trunk.

    >>> operandTwo "-term" "-relmap" [] -}
operandTwo :: String -> String -> [String] -> RopOperand
operandTwo a b ns = (trunkBy f, [a,b], ns) where
    f [x, y]      = Right [ (a, [x]), (b, [y]) ]
    f _           = Left $ B.abortMalformedOperand "binary"

{-| One-and-multiple-element trunk.

    >>> operandOneList "-pattern" "-term" [] -}
operandOneList :: String -> String -> [String] -> RopOperand
operandOneList a b ns = (trunkBy f, [a,b], ns) where
    f (x:xs)          = Right [ (a, [x]), (b, xs) ]
    f _               = Left $ B.abortMalformedOperand "uncons"

{-| Give a name to unnamed operand. -}
trunkBy :: RopFullSorter -> RopSorter
trunkBy f xs = case lookup "" xs of
                 Just x  -> Right . (++ xs) =<< f x
                 Nothing -> Right xs

