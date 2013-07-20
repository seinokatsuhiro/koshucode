{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Naming
( -- * prefix
  relopPrefix, relmapPrefix, relPrefix

  -- * unprefix
, relopUnprefix, relmapUnprefix, relUnprefix

  -- * prefix-change
, relopPrefixChange, relmapPrefixChange, relPrefixChange
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini
import qualified Data.List as List



-- ----------------------  prefix

relopPrefix :: Kit.Relop VContent
relopPrefix use = do
  pre <- Mini.getTerm use "-prefix"
  ns  <- Mini.getTerms use "-term"
  Right $ relmapPrefix use pre ns

relmapPrefix :: OpUse v -> String -> [String] -> Relmap v
relmapPrefix use pre ns = Kit.relmapCalc use "prefix" sub where
    sub _ r1 = relPrefix pre ns r1

{-| Add prefix to terms. -}
relPrefix
    :: String         -- ^ Prefix text
    -> [String]       -- ^ Changing term names
    -> AbMap (Rel v)  -- ^ Relation to relation
relPrefix pre ns (Rel h1 b1) = Right $ Rel h2 b1 where
    h2 = Kit.rehead (map f) h1
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

relopUnprefix :: Kit.Relop VContent
relopUnprefix use = do
  pre <- Mini.getTerm use "-prefix"
  Right $ relmapUnprefix use pre

relmapUnprefix :: OpUse v -> String -> Relmap v
relmapUnprefix use pre = Kit.relmapCalc use "unprefix" sub where
    sub _ r1 = relUnprefix pre r1

{-| Remove prefix -}
relUnprefix
    :: String         -- ^ Prefix text
    -> AbMap (Rel v)  -- ^ Relation to relation
relUnprefix pre (Rel h1 b1) = Right $ Rel h2 b1 where
    h2 = Kit.rehead (map $ unprefixName pre) h1

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

relopPrefixChange :: Kit.Relop VContent
relopPrefixChange use = do
  new <- Mini.getTerm use "-new"
  old <- Mini.getTerm use "-old"
  Right $ relmapPrefixChange use new old

relmapPrefixChange :: OpUse v -> String -> String -> Relmap v
relmapPrefixChange use new old =
    Kit.relmapCalc use "prefix-change" sub
    where sub _ r1 = relPrefixChange new old r1

{-| Change prefix -}
relPrefixChange
    :: String         -- ^ New prefix
    -> String         -- ^ Old prefix
    -> AbMap (Rel v)  -- ^ Relation to relation
relPrefixChange new old (Rel h1 b1) = Right $ Rel h2 b1 where
    h2   = Kit.rehead (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

