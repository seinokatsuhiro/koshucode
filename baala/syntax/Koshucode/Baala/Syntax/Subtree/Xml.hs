{-# OPTIONS_GHC -Wall #-}

-- | XML tree.

module Koshucode.Baala.Syntax.Subtree.Xml
  ( XmlTree,
    XmlToken (..),
    XmlTerm,
    XmlKey (..),
    xmlTokens,
    xmlTrees,
  ) where

import qualified Text.HTML.TagSoup                       as X
import qualified Text.StringLike                         as X
import qualified Koshucode.Baala.Base                    as B

-- | XML tree.
type XmlTree s = B.RawTree () (XmlTerm s) (XmlTerm s)

type XmlTree' s = B.CodeTree (XmlTerm s) (XmlToken s)

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
    = XmlElem         -- ^ Element node
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

-- | Convert XML tokens to XML tree.
--
--   >>> mapM_ putStrLn . B.ppRawTrees =<< B.abortLeft (xmlTrees $ xmlTokens "<hello>wonderful</world>")
--   > () (XmlElem,"hello")
--     - (XmlText,"wonderful")
--
--   >>> mapM_ putStrLn . B.ppRawTrees =<< B.abortLeft (xmlTrees $ xmlTokens "<a x='xx' y='yy'>")
--   > () (XmlElem,"a")
--     - (XmlAttr "x","xx")
--     - (XmlAttr "y","yy")
--
xmlTrees :: (Ord s, X.StringLike s) => [XmlToken s] -> B.Ab [XmlTree s]
xmlTrees toks =
    do trees <- B.codeTrees bracket B.BracketNone toks
       Right (modifyTree <$> trees)
    where
      bracket (XmlOpen n)   = B.BracketOpen  (XmlElem, n)
      bracket (XmlClose n)  = B.BracketClose (XmlElem, n)
      bracket _             = B.BracketNone

modifyTree :: (X.StringLike s) => XmlTree' s -> XmlTree s
modifyTree = B.treeMap (const ()) y xmlUntoken where
    y (Just (t, _))  = xmlUntoken t
    y _              = (XmlComment, X.empty)

xmlUntoken :: XmlToken s -> XmlTerm s
xmlUntoken (XmlOpen  n) = (XmlElem, n)
xmlUntoken (XmlClose n) = (XmlElem, n)
xmlUntoken (XmlTerm  z) = z

