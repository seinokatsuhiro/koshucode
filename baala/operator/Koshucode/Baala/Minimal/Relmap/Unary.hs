{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

-- | Unary relational mappers

module Koshucode.Baala.Minimal.Relmap.Unary
( -- * Projection
  project
, relEmpty, relmapEmpty

  -- * Naming
, renameNP
, prefix
, unprefix
, prefixChange

  -- * Current
, enclose
, conf
, size
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Minimal.OpeKit as Kit
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple



-- ----------------------  projection

project :: (Ord v) => ([Int] -> Listmap v) -> [String] -> a -> RelmapFun v
project f ns2 _ (Rel h1 b1) = Rel h2 b2 where
    pos = List.sort $ Kit.headPoss h1 (map singleton ns2)
    pj  = f $ Kit.posPoss pos
    h2  = Kit.rehead pj h1
    b2  = unique $ map pj b1

-- | Throw away all tuples in a relation.
relEmpty :: Rel v -> Rel v
relEmpty (Rel h1 _) = Rel h1 []

relmapEmpty :: Kit.OpUse v -> Kit.Relmap v
relmapEmpty use = Kit.relmapCalc use "empty" sub where
    sub _ = relEmpty



-- ----------------------  naming

-- | Change name of terms.
renameNP :: [(String, String)] -> a -> RelmapFun v
renameNP np _ (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map re) h1
    pn = map Tuple.swap np
    re p = Maybe.fromMaybe p $ lookup p pn

-- | Add prefix to terms
prefix :: String -> Kit.Relmap v
prefix ns = flow "prefix" $ Kit.withP prefix2 ns

flow :: String -> RelmapFun v -> Kit.Relmap v
flow = undefined

prefix2 :: [String] -> Rel v -> Rel v
prefix2 (p:ns) (Rel h1 b1) = Rel h2 b1 where
    h2  = Kit.rehead (map f) h1
    f n | n `elem` ns = prefixName p n
        | otherwise   = n
prefix2 _ _ = undefined

-- | Remove prefix
unprefix :: String -> Kit.Relmap v
unprefix n = flow "unprefix" $ Kit.withP unprefix2 n

unprefix2 :: [String] -> Rel v -> Rel v
unprefix2 [p] (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map $ unprefixName p) h1
unprefix2 _ _ = undefined

-- | Change prefix
prefixChange :: String -> Kit.Relmap v
prefixChange np = flow "prefixChange" $ Kit.withP prefixChange2 np

prefixChange2 :: [[Char]] -> Rel v -> Rel v
prefixChange2 [n,p] (Rel h1 b1) = Rel h2 b1 where
    h2  = Kit.rehead (map f) h1
    old = p ++ "-"
    new = n ++ "-"
    f n' = case List.stripPrefix old n' of
             Just n2 -> new ++ n2
             Nothing -> n'
prefixChange2 _ _ = undefined

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  current

-- | Enclose the current relation in a term.
enclose :: (RelValue v) => String -> Kit.Relmap v
enclose n = flow "enclose" $ Kit.withN1 enclose2 n

enclose2 :: (RelValue v) => [String] -> Rel v -> Rel v
enclose2 [n] r@(Rel h1 _) = Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[relValue r]]
enclose2 _ _ = undefined

-- | Current term configuration.
conf :: (StringValue v) => String -> Kit.Relmap v
conf n = flow "conf" $ Kit.withN1 conf2 n

conf2 :: (StringValue v) => [String] -> Rel t -> Rel v
conf2 [n] (Rel h1 _) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[stringValue $ show s]]
    s  = show $ docParen $ doc h1
conf2 _ _ = undefined

-- | Current cardinality.
size :: (IntValue v) => String -> Kit.Relmap v
size n = flow "size" $ Kit.withN1 size2 n

size2 :: (IntValue v) => [String] -> Rel t -> Rel v
size2 [n] (Rel _ b1) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[intValue $ length b1]]
size2 _ _ = undefined

