{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Naming
( -- * prefix
  consPrefix, relmapPrefix, relkitPrefix,
  -- * unprefix
  consUnprefix, relmapUnprefix, relkitUnprefix,
  -- * prefix-change
  consPrefixChange, relmapPrefixChange, relkitPrefixChange,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  prefix

consPrefix :: C.RopCons c
consPrefix use =
    do pre <- Rop.getTerm  use "-prefix"
       ns  <- Rop.getTerms use "-term"
       Right $ relmapPrefix use pre ns

relmapPrefix :: C.RopUse c -> String -> [String] -> C.Relmap c
relmapPrefix use pre ns = C.relmapFlow use $ relkitPrefix pre ns

{-| Add prefix to specified terms. -}
relkitPrefix
    :: String             -- ^ Prefix text (except for hyphen)
    -> [String]           -- ^ Changing termnames
    -> C.RelkitCalc c
relkitPrefix pre ns h1 = Right $ C.relkit h2 C.RelkitId where
    h2 = B.headChange (map f) h1
    f n | n `elem` ns  = prefixName pre n
        | otherwise    = n

prefixName :: String -> String -> String
prefixName pre ('/' : ns) = pre ++ "-" ++ ns
prefixName _ _ = undefined



-- ----------------------  unprefix

consUnprefix :: C.RopCons c
consUnprefix use =
    do pre <- Rop.getTerm use "-prefix"
       Right $ relmapUnprefix use pre

relmapUnprefix :: C.RopUse c -> String -> C.Relmap c
relmapUnprefix use = C.relmapFlow use . relkitUnprefix

{-| Remove prefix -}
relkitUnprefix
    :: String             -- ^ Prefix text (except for hyphen)
    -> C.RelkitCalc c
relkitUnprefix pre h1 = Right $ C.relkit h2 C.RelkitId where
    h2 = B.headChange (map $ unprefixName pre) h1

unprefixName :: String -> String -> String
unprefixName pre n =
    case List.stripPrefix pre n of
      Just ('-' : n2) -> '/' : n2
      _ -> n



-- ----------------------  prefix-change

consPrefixChange :: C.RopCons c
consPrefixChange use =
    do new <- Rop.getTerm use "-new"
       old <- Rop.getTerm use "-old"
       Right $ relmapPrefixChange use new old

relmapPrefixChange :: C.RopUse c -> String -> String -> C.Relmap c
relmapPrefixChange use new old = C.relmapFlow use $ relkitPrefixChange new old

{-| Change prefix -}
relkitPrefixChange
    :: String             -- ^ New prefix text (except for hyphen)
    -> String             -- ^ Old prefix text (except for hyphen)
    -> C.RelkitCalc c
relkitPrefixChange new old h1 = Right $ C.relkit h2 C.RelkitId where
    h2   = B.headChange (map f) h1
    new' = new ++ "-"
    old' = old ++ "-"
    f n' = case List.stripPrefix old' n' of
             Just n2 -> new' ++ n2
             Nothing -> n'

