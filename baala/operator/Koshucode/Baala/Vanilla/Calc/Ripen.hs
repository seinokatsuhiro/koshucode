{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Calc.Ripen
( calcRipen
, calcBinary
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Data.List as List
import qualified Data.Map  as Map

calcRipen :: Ripen Token Val
calcRipen (TWord _ q n) xs arg
    | q > 0     = stringValue n
    | otherwise = case Map.lookup n operators of
                    Just g  -> g xs arg
                    Nothing -> stringValue n
calcRipen (TTermP _ ps) _ arg = termRef ps arg
calcRipen f xs _ = error $ "unknown token: "
                     ++ show f ++ " " ++ show xs

termRef :: [Int] -> Calc Val
termRef []     _ = nil
termRef (-1:_) _ = nil
termRef (p:ps)  arg =
    case arg !! p of
      Relv (Rel _ args) -> listValue $ map (termRef ps) args
      v -> v

type OpSet v = Map.Map String ([v] -> Calc v)

operators :: OpSet Val
operators = Map.fromList
  [ ("=",     fbin (==)),
    ("/=",    fbin (/=)),
    ("<",     fbin (<)),
    (">",     fbin (>)),
    ("<=",    fbin (<=)),
    (">=",    fbin (>=)),

    ("or",    for),
    ("and",   fand),

    ("+",     fintv List.sum),
    ("*",     fintv List.product),
    ("-",     fint2 (-)),
    ("abs",   fint1 abs),

    ("list",  flist),
    ("size",  fsize),
    ("total", ftotal),
    ("min",   fmin),
    ("max",   fmax)
  ] where

    fbin p [x,y] _ = boolValue (p x y)
    fbin _ _ _     = undefined

    for  [Boolv x, Boolv y] _ = boolValue (x || y)
    for   _ _ = undefined
    fand [Boolv x, Boolv y] _ = boolValue (x && y)
    fand _ _ = undefined

    fint1 f [x]   _ = intValue (f $ toInt x)
    fint1 _ _ _     = undefined
    fint2 f [x,y] _ = intValue (f (toInt x) (toInt y))
    fint2 _ _ _     = undefined
    fintv f xs    _ = intValue (f $ map toInt xs)
    --farg  n _     arg = arg !! n

    flist  xs _ = listValue xs

    fsize [Stringv xs] _ = intValue (length xs)
    fsize [Listv   xs] _ = intValue (length xs)
    fsize [Relv (Rel _ b)] _ = intValue (length b)
    fsize _ _ = undefined

    ftotal [Listv xs] _ = intValue (sum $ map toInt xs)
    ftotal _ _ = undefined
    fmin   [Listv xs] _ = intValue (minimum $ map toInt xs)
    fmin   _ _ = undefined
    fmax   [Listv xs] _ = intValue (maximum $ map toInt xs)
    fmax   _ _ = undefined

-- | Convert infix form to prefix form
calcBinary :: TokenTree -> TokenTree
calcBinary = binaryTree ht where
    op = map (TWord 0 0) . words
    ht = heightTable [
          (Right 8, op "or"),
          (Right 7, op "and"),
          (Right 6, op "= <>"),
          (Right 2, op "+ -"),
          (Right 1, op "* /")
         ]

