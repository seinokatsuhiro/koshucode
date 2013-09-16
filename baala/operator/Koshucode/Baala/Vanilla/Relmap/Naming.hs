{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Naming
( -- * prefix
  ropConsPrefix, relmapPrefix, relgenPrefix,
  -- * unprefix
  ropConsUnprefix, relmapUnprefix, relgenUnprefix,
  -- * prefix-change
  ropConsPrefixChange, relmapPrefixChange, relgenPrefixChange,
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
relmapPrefix use pre ns = C.relmapCalc use "prefix" gen where
    gen _ = relgenPrefix pre ns

{-| Add prefix to specified terms. -}
relgenPrefix
    :: String              -- ^ Prefix text (except for hyphen)
    -> [String]            -- ^ Changing termnames
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenPrefix pre ns h1 = Right $ C.Relgen h2 (C.RelgenOneToOne id) where
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
relmapUnprefix use pre = C.relmapCalc use "unprefix" gen where
    gen _ = relgenUnprefix pre

{-| Remove prefix -}
relgenUnprefix
    :: String             -- ^ Prefix text (except for hyphen)
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)  -- ^ Generator for output relation
relgenUnprefix pre h1 = Right $ C.Relgen h2 (C.RelgenOneToOne id) where
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
    C.relmapCalc use "prefix-change" gen
     where gen _ = relgenPrefixChange new old

{-| Change prefix -}
relgenPrefixChange
    :: String             -- ^ New prefix text (except for hyphen)
    -> String             -- ^ Old prefix text (except for hyphen)
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)  -- ^ Generator for output relation
relgenPrefixChange new old h1 = Right $ C.Relgen h2 (C.RelgenOneToOne id) where
    h2   = B.headChange (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

