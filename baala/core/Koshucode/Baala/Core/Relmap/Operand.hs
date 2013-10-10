{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Operand
( -- * Datatype
  RopOperand,
  RopSorter,
  RopFullSorter,

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
    =  [B.TokenTree]                 -- ^ Unsorted operand
    -> B.Ab [B.Named [B.TokenTree]]  -- ^ Fully sorted operand

type RopSorter
    =  [B.Named [B.TokenTree]]       -- ^ Basically sorted operand
    -> B.Ab [B.Named [B.TokenTree]]  -- ^ Fully sorted operand

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
    f _         = Left $ B.AbortOpeandUnmatch "unary"

{-| Two-element trunk.

    >>> operandTwo "-term" "-relmap" [] -}
operandTwo :: String -> String -> [String] -> RopOperand
operandTwo a b ns = (trunkBy f, [a,b], ns) where
    f [x, y]      = Right [ (a, [x]), (b, [y]) ]
    f _           = Left $ B.AbortOpeandUnmatch "binary"

{-| One-and-multiple-element trunk.

    >>> operandOneList "-pattern" "-term" [] -}
operandOneList :: String -> String -> [String] -> RopOperand
operandOneList a b ns = (trunkBy f, [a,b], ns) where
    f (x:xs)          = Right [ (a, [x]), (b, xs) ]
    f _               = Left $ B.AbortOpeandUnmatch "uncons"

{-| Give a name to unnamed operand. -}
trunkBy :: RopFullSorter -> RopSorter
trunkBy f xs = case lookup "" xs of
                 Just x  -> Right . (++ xs) =<< f x
                 Nothing -> Right xs

