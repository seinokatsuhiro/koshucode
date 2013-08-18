{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Naming
( -- * prefix
  ropConsPrefix, relmapPrefix, relPrefix,
  -- * unprefix
  ropConsUnprefix, relmapUnprefix, relUnprefix,
  -- * prefix-change
  ropConsPrefixChange, relmapPrefixChange, relPrefixChange,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import Koshucode.Baala.Vanilla.Type



-- ----------------------  prefix

ropConsPrefix :: C.RopCons VContent
ropConsPrefix use =
    do pre <- Builtin.getTerm  use "-prefix"
       ns  <- Builtin.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapCalc use "prefix" sub where
    sub _ r1 = relPrefix pre ns r1

{-| Add prefix to terms. -}
relPrefix
    :: String             -- ^ Prefix text
    -> [String]           -- ^ Changing term names
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relPrefix pre ns (B.Rel h1 b1) = Right $ B.Rel h2 b1 where
    h2 = B.headChange (map f) h1
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

ropConsUnprefix :: C.RopCons VContent
ropConsUnprefix use =
    do pre <- Builtin.getTerm use "-prefix"
       Right $ relmapUnprefix use pre

relmapUnprefix :: C.RopUse c -> String -> C.Relmap c
relmapUnprefix use pre = C.relmapCalc use "unprefix" sub where
    sub _ r1 = relUnprefix pre r1

{-| Remove prefix -}
relUnprefix
    :: String             -- ^ Prefix text
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relUnprefix pre (B.Rel h1 b1) = Right $ B.Rel h2 b1 where
    h2 = B.headChange (map $ unprefixName pre) h1

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

ropConsPrefixChange :: C.RopCons VContent
ropConsPrefixChange use =
    do new <- Builtin.getTerm use "-new"
       old <- Builtin.getTerm use "-old"
       Right $ relmapPrefixChange use new old

relmapPrefixChange :: C.RopUse c -> String -> String -> C.Relmap c
relmapPrefixChange use new old =
    C.relmapCalc use "prefix-change" sub
    where sub _ r1 = relPrefixChange new old r1

{-| Change prefix -}
relPrefixChange
    :: String           -- ^ New prefix
    -> String           -- ^ Old prefix
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relPrefixChange new old (B.Rel h1 b1) = Right $ B.Rel h2 b1 where
    h2   = B.headChange (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

