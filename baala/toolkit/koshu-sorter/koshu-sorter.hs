#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}
-- ------------------------------------------------------------------
--
--  DESCRIPTION
--    Display results of operand sorters
--
--  USAGE
--    chmod 755 koshu-sorter.hs
--    echo "minimal source A /x /y" | ./koshu-sorter.hs
--
-- ------------------------------------------------------------------

import qualified Text.PrettyPrint         as P
import qualified Koshucode.Baala.Base     as B
import qualified Koshucode.Baala.Core     as C
import qualified Koshucode.Baala.Minimal  as Minimal
import qualified Koshucode.Baala.Vanilla  as Vanilla


main :: IO ()
main = interact translate where
    translate :: String -> String
    translate = unlines . concatMap applySorter . lines

    applySorter :: String -> [String]
    applySorter ('*' : _) = []
    applySorter ""        = []
    applySorter line      = [show $ lineDoc line, ""]

lineDoc :: String -> B.Doc
lineDoc line = B.doc line P.$$ body where
    body = P.nest 2 (uncons $ B.tt line)

    uncons (B.TreeL (B.TWord _ _ k) : xs) = group k xs
    uncons _ = skip line

    group k xs =
        case lookup k allSorters of
          Just s  -> sorterDoc s line xs
          Nothing -> unk line

skip, unk :: String -> B.Doc
skip line = B.doc $ "** SKIP: "    ++ line
unk  line = B.doc $ "** UNKNOWN: " ++ line

sorterDoc :: [B.Named C.RopSorter] -> String -> [B.TokenTree] -> B.Doc
sorterDoc sorters line = uncons where
    uncons (B.TreeL (B.TWord _ _ k) : xs) =
        case lookup k sorters of
          Just s  -> sdoc s xs
          Nothing -> unk line
    uncons _ = skip line

    sdoc sorter = namedTtDoc . sorter . C.sortOperand

namedTtDoc :: [B.Named [B.TokenTree]] -> B.Doc
namedTtDoc = B.docv . map d where
    d ("", _)  = P.empty
    d (n,  xs) = B.doc n P.$$ P.nest 3 (B.ttDoc xs)


-- ----------------------  Sorters

allSorters :: [B.Named [B.Named C.RopSorter]]
allSorters =
    [ ("minimal",  minimalSorters)
    , ("vanilla",  vanillaSorters) ]

minimalSorters :: [B.Named C.RopSorter]
minimalSorters =
    [ ("meet",          Minimal.likeMeet)
    , ("pick",          Minimal.likePick)
    , ("source",        Minimal.likeSource) ]

vanillaSorters :: [B.Named C.RopSorter]
vanillaSorters =
    [ ("group",         Vanilla.likeGroup)
    , ("pos",           Vanilla.likePos)
    , ("prefix",        Vanilla.likePrefix)
    , ("prefixChange",  Vanilla.likePrefixChange)
    , ("size",          Vanilla.likeSize)
    , ("unprefix",      Vanilla.likeUnprefix) ]

