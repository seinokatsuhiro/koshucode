{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Naming
( -- * Prefix
  relmapPrefix
, relPrefix

  -- * Unprefix
, relmapUnprefix
, relUnprefix

  -- * Prefix change
, relmapPrefixChange
, relPrefixChange
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import qualified Data.List as List



-- ----------------------  Prefix

relmapPrefix :: OpUse v -> String -> [String] -> Relmap v
relmapPrefix use pre ns = Kit.relmapCalc use "prefix" sub where
    sub _ r1 = relPrefix pre ns r1

{-| Add prefix to terms. -}
relPrefix
    :: String       -- ^ Prefix text
    -> [String]     -- ^ Changing term names
    -> Map (Rel v)  -- ^ Relation to relation
relPrefix pre ns (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map f) h1
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  Unprefix

relmapUnprefix :: OpUse v -> String -> Relmap v
relmapUnprefix use pre = Kit.relmapCalc use "unprefix" sub where
    sub _ r1 = relUnprefix pre r1

{-| Remove prefix -}
relUnprefix
    :: String       -- ^ Prefix text
    -> Map (Rel v)  -- ^ Relation to relation
relUnprefix pre (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map $ unprefixName pre) h1

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  Change prefix

relmapPrefixChange :: OpUse v -> String -> String -> Relmap v
relmapPrefixChange use new old =
    Kit.relmapCalc use "prefix-change" sub
    where sub _ r1 = relPrefixChange new old r1

{-| Change prefix -}
relPrefixChange
    :: String      -- ^ New prefix
    -> String      -- ^ Old prefix
    -> Map (Rel v) -- ^ Relation to relation
relPrefixChange new old (Rel h1 b1) = Rel h2 b1 where
    h2   = Kit.rehead (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

