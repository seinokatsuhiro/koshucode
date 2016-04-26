{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute sorter.

module Koshucode.Baala.Data.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data.Attr.Attr     as D
import qualified Koshucode.Baala.Data.Attr.AttrPos  as D

-- | Parse attribute sorter string.
--
-- >>> parseAttrLayout "0 | -c"
-- AttrLayout { positional = AttrPos0,
--                   named = [AttrNormal "c"] }
--
-- >>> parseAttrLayout "2 -a -b | -c"
-- AttrLayout { positional = AttrPos2 (AttrNormal "a") (AttrNormal "b"),
--                   named = [AttrNormal "c"] }

parseAttrLayout :: String -> D.AttrLayout
parseAttrLayout s = attr where
    n = map attrName
    attr = case map words $ B.divideBy (== '|') s of
             [q : trunk]          -> attrDef q (n trunk) []
             [q : trunk, branch]  -> attrDef q (n trunk) (n branch)
             _                    -> attrBug s

attrName :: String -> D.AttrName
attrName ('-' : n) | l == '^'    = attrLocal i
                   | l == '/'    = D.AttrRelmapNormal i  -- "-xxx/"
                   | otherwise   = D.AttrNormal       n  -- "-xxx"
                   where l = last n
                         i = init n
attrName n = attrBug n

attrLocal :: String -> D.AttrName
attrLocal n        | l == '/'    = D.AttrRelmapLocal i    -- "-xxx/^"
                   | otherwise   = D.AttrNormal      n    -- "-xxx^"
                   where l = last n
                         i = init n

attrDef :: String -> [D.AttrName] -> [D.AttrName] -> D.AttrLayout
attrDef q ns = D.attrLayout $ attrPosDef q ns

attrPosDef :: String -> [D.AttrName] -> D.AttrNamePos
attrPosDef "E"  as         = D.AttrPosE as
attrPosDef "0"  []         = D.AttrPos0
attrPosDef "1"  [a]        = D.AttrPos1  a
attrPosDef "2"  [a,b]      = D.AttrPos2  a b
attrPosDef "3"  [a,b,c]    = D.AttrPos3  a b c
attrPosDef "4"  [a,b,c,d]  = D.AttrPos4  a b c d
attrPosDef "1?" [a,b]      = D.AttrPos1Q a b
attrPosDef "V"  [a]        = D.AttrPosV  a
attrPosDef "1V" [a,b]      = D.AttrPos1V a b
attrPosDef _ xs            = attrBug $ unwords $ map D.attrNameText xs

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

