{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Naming
( -- * prefix
  ropConsPrefix, relmapPrefix, relfyPrefix,
  -- * unprefix
  ropConsUnprefix, relmapUnprefix, relfyUnprefix,
  -- * prefix-change
  ropConsPrefixChange, relmapPrefixChange, relfyPrefixChange,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  prefix

ropConsPrefix :: C.RopCons c
ropConsPrefix use =
    do pre <- Rop.getTerm  use "-prefix"
       ns  <- Rop.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapCalc use $ relfyPrefix pre ns

{-| Add prefix to specified terms. -}
relfyPrefix
    :: String             -- ^ Prefix text (except for hyphen)
    -> [String]           -- ^ Changing termnames
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfyPrefix pre ns h1 = Right $ C.relfy h2 C.RelfyId where
    h2 = B.headChange (map f) h1
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

ropConsUnprefix :: C.RopCons c
ropConsUnprefix use =
    do pre <- Rop.getTerm use "-prefix"
       Right $ relmapUnprefix use pre

relmapUnprefix :: C.RopUse c -> String -> C.Relmap c
relmapUnprefix use pre = C.relmapCalc use $ relfyUnprefix pre

{-| Remove prefix -}
relfyUnprefix
    :: String             -- ^ Prefix text (except for hyphen)
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)  -- ^ Generator for output relation
relfyUnprefix pre h1 = Right $ C.relfy h2 C.RelfyId where
    h2 = B.headChange (map $ unprefixName pre) h1

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

ropConsPrefixChange :: C.RopCons c
ropConsPrefixChange use =
    do new <- Rop.getTerm use "-new"
       old <- Rop.getTerm use "-old"
       Right $ relmapPrefixChange use new old

relmapPrefixChange :: C.RopUse c -> String -> String -> C.Relmap c
relmapPrefixChange use new old = C.relmapCalc use $ relfyPrefixChange new old

{-| Change prefix -}
relfyPrefixChange
    :: String             -- ^ New prefix text (except for hyphen)
    -> String             -- ^ Old prefix text (except for hyphen)
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)  -- ^ Generator for output relation
relfyPrefixChange new old h1 = Right $ C.relfy h2 C.RelfyId where
    h2   = B.headChange (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

