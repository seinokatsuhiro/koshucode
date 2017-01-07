{-# OPTIONS_GHC -Wall #-}

-- | XML tree.

module Koshucode.Baala.Syntax.Subtree.Xml
  ( -- * XML token
    XmlToken (..),
    XmlTerm,
    XmlKey (..),
    xmlTokens,

    -- * XML tree
    XmlTree,
    xmlTrees,

    -- * Conversion
    XmlTreeOutput,
    xmlSubtree,
    xmlData,
  ) where

import qualified Text.HTML.TagSoup                       as X
import qualified Text.StringLike                         as X
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Symbol           as S
import qualified Koshucode.Baala.Syntax.Subtree.Subtree  as S


-- ============================================  XML token

-- | XML token.
data XmlToken s
    = XmlOpen  s            -- ^ Open tag
    | XmlClose s            -- ^ Close tag
    | XmlTerm (XmlTerm s)   -- ^ Terminal token
      deriving (Show, Eq, Ord)

instance B.GetCodePos (XmlToken s) where
    getCP  _ = B.def
    getCPs _ = []

-- | Terminal of XML tree.
type XmlTerm s = (XmlKey s, s)

-- | Key data of XML tree.
data XmlKey s
    = XmlElem s       -- ^ Element node
    | XmlAttr s       -- ^ Attribute of element
    | XmlText         -- ^ Text node
    | XmlComment      -- ^ Comment node
      deriving (Show, Eq, Ord)

-- | Tokenize XML document.
--
--   >>> xmlTokens "<hello>wonderful</world>"
--   [XmlOpen "hello", XmlTerm (XmlText, "wonderful"), XmlClose "hello"]
--
--   >>> xmlTokens "<a x='xx' y='yy'>"
--   [XmlOpen "a", XmlTerm (XmlAttr "x", "xx"), XmlTerm (XmlAttr "y", "yy"), XmlClose "a"]
--
xmlTokens :: (X.StringLike s) => s -> [XmlToken s]
xmlTokens = convertTags . X.parseTags

convertTags :: (X.StringLike s) => [X.Tag s] -> [XmlToken s]
convertTags = loop [] where
    loop up (X.TagOpen n xs : tags) = XmlOpen n : ((attr <$> xs)
                                                   ++ loop (n : up) tags)
    loop (u : up) tags'@(X.TagClose n : tags)
                       | u == n     = XmlClose n   : loop up tags
                       | otherwise  = XmlClose u   : loop up tags'
    loop up (X.TagText s    : tags) = XmlTerm (XmlText, s) : loop up tags
    loop up (X.TagComment s : tags) = XmlTerm (XmlComment, s) : loop up tags
    loop up (_              : tags) = loop up tags
    loop up []                      = XmlClose <$> up

    attr (n, v) = XmlTerm (XmlAttr n, v)


-- ============================================  XML tree

-- | XML tree.
type XmlTree s = S.Subtree (XmlTerm s)

type XmlTree' s = B.CodeTree (XmlTerm s) (XmlToken s)

-- | Convert XML tokens to XML tree.
--
--   >>> B.printTrees =<< B.abortLeft (xmlTrees $ xmlTokens "<hello>wonderful</world>")
--   > () (XmlElem "hello","hello")
--     - (XmlText,"wonderful")
--
--   >>> B.printTrees =<< B.abortLeft (xmlTrees $ xmlTokens "<a x='xx' y='yy'>")
--   > () (XmlElem "a","a")
--     - (XmlAttr "x","xx")
--     - (XmlAttr "y","yy")
--
xmlTrees :: (Ord s, X.StringLike s) => [XmlToken s] -> B.Ab [XmlTree s]
xmlTrees toks =
    do trees <- B.codeTrees bracket B.BracketNone toks
       Right (modifyTree <$> trees)
    where
      bracket (XmlOpen n)   = B.BracketOpen  (XmlElem n, n)
      bracket (XmlClose n)  = B.BracketClose (XmlElem n, n)
      bracket _             = B.BracketNone

modifyTree :: (X.StringLike s) => XmlTree' s -> XmlTree s
modifyTree = B.treeMap (const []) y xmlUntoken where
    y (Just (t, _))  = xmlUntoken t
    y _              = (XmlComment, X.empty)

xmlUntoken :: XmlToken s -> XmlTerm s
xmlUntoken (XmlOpen  n) = (XmlElem n, n)
xmlUntoken (XmlClose n) = (XmlElem n, n)
xmlUntoken (XmlTerm  z) = z


-- ============================================  Conversion

-- | XML output tree.
type XmlTreeOutput s = S.SubtreeOutput (XmlTerm s)

-- | Calculate subtree of XML tree.
xmlSubtree :: [S.SubtreePattern] -> [XmlTree String] -> [XmlTreeOutput String]
xmlSubtree = S.subtreeWith xmlKeyString

xmlKeyString :: XmlTerm String -> String
xmlKeyString (XmlElem s  , _) = s
xmlKeyString (XmlAttr s  , _) = s
xmlKeyString (XmlText    , _) = "@text"
xmlKeyString (XmlComment , _) = "@comment"

-- | Extract data from XML output tree.
xmlData :: XmlTreeOutput String -> [(S.JudgeClass, [S.Term O.Value])]
xmlData = S.subtreeData . xmlValue

xmlValue :: XmlTreeOutput String -> S.SubtreeOutput O.Value
xmlValue = snd . loop 1 where
    loop i (B.TreeL z)      = (i + 1, B.TreeL (value i z))
    loop i (B.TreeB b y xs) = let (i', xs') = branch (i + 1) [] xs
                              in (i', B.TreeB b (value i y) xs')

    value _ (S.SubtreeNone, _)            = (S.SubtreeNone, O.VEmpty)
    value _ (S.SubtreeText cs n, (_, s))  = (S.SubtreeText cs n, O.VStr $ O.trimBoth s)
    value i (S.SubtreeSeq  cs n, _)       = (S.SubtreeSeq  cs n, O.VInt i)

    branch i xs' [] = (i, reverse xs')
    branch i xs' (x : xs) = case loop i x of
                              (i', x') -> branch i' (x' : xs') xs

