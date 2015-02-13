#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}
-- ------------------------------------------------------------------
--
--  DESCRIPTION
--    Display results of operand sorters.
--    Display tokens or token trees.
--
--  USAGE
--    chmod 755 koshu-sorter.hs
--    echo "minimal source A /x /y" | ./koshu-sorter.hs
--    echo "token a b (c [d e])" | ./koshu-sorter.hs
--    echo "tree a b (c [d e])" | ./koshu-sorter.hs
--
--  SYNTAX
--    minimal SORTER OPERAND ...
--      Show result of operand sorter defined in Minimal module
--    vanilla SORTER OPERAND ...
--      Show result of operand sorter defined in Vanilla module
--    tree INPUT ...
--      Show token trees of input
--    token INPUT ...
--      Show tokens of input
--
-- ------------------------------------------------------------------

import qualified Data.Char                   as C
import qualified Text.PrettyPrint            as P
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Minimal  as Minimal
import qualified Koshucode.Baala.Op.Vanilla  as Vanilla


main :: IO ()
main = interact f where
    f :: String -> String
    f = unlines . concatMap g . B.linesCrlf

    g :: String -> [String]
    g ('*' : _) = []
    g ""        = []
    g line      = [show $ lineDoc line, ""]

lineDoc :: String -> B.Doc
lineDoc line = B.doc line P.$$ P.nest 2 body where
    (g, xs)   = unconsWord line
    (n, xs2)  = unconsWord xs
    toks      = B.tokens xs
    tt        = B.tt xs2
    body | g == "tree"  = B.ttDoc tt
         | g == "token" = B.docv toks
         | otherwise    = case lookup g allSorters of
                            Just s  -> sorterDoc s line n tt
                            Nothing -> unk line

sorterDoc :: [B.Named C.RopSorter]
          -> String -> String -> [B.TTree] -> B.Doc
sorterDoc sorters line name tt =
    case lookup name sorters of
      Just s  -> namedTtDoc . s . C.sortOperand $ tt
      Nothing -> unk line

skip, unk :: String -> B.Doc
skip line = B.doc $ "** SKIP: "    ++ line
unk  line = B.doc $ "** UNKNOWN: " ++ line

unconsWord :: String -> (String, String)
unconsWord xs = (word, rest') where
    (word, rest) = span (not . C.isSpace) xs
    rest'        = dropWhile C.isSpace rest

namedTtDoc :: [B.NamedTrees] -> B.Doc
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

