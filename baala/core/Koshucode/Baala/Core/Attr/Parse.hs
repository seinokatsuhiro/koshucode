{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute sorter.

module Koshucode.Baala.Core.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Core.Attr.Attr     as C
import qualified Koshucode.Baala.Core.Attr.AttrPos  as C

-- | Parse attribute sorter string.
--
-- >>> parseAttrLayout "0 | -c"
-- AttrLayout { positional = AttrPos0,
--                   named = [AttrNormal "c"] }
--
-- >>> parseAttrLayout "2 -a -b | -c"
-- AttrLayout { positional = AttrPos2 (AttrNormal "a") (AttrNormal "b"),
--                   named = [AttrNormal "c"] }

parseAttrLayout :: String -> C.AttrLayout
parseAttrLayout s = attr where
    n = map attrName
    attr = case map words $ B.divideBy (== '|') s of
             [q : trunk]          -> attrDef q (n trunk) []
             [q : trunk, branch]  -> attrDef q (n trunk) (n branch)
             _                    -> attrBug s

attrName :: String -> C.AttrName
attrName ('-' : n) | l == '^'    = attrLocal i
                   | l == '/'    = C.AttrRelmapNormal i  -- "-xxx/"
                   | otherwise   = C.AttrNormal       n  -- "-xxx"
                   where l = last n
                         i = init n
attrName n = attrBug n

attrLocal :: String -> C.AttrName
attrLocal n        | l == '/'    = C.AttrRelmapLocal i    -- "-xxx/^"
                   | otherwise   = C.AttrNormal      n    -- "-xxx^"
                   where l = last n
                         i = init n

attrDef :: String -> [C.AttrName] -> [C.AttrName] -> C.AttrLayout
attrDef q ns = C.attrLayout $ attrPosDef q ns

attrPosDef :: String -> [C.AttrName] -> C.AttrNamePos
attrPosDef "E"  as         = C.AttrPosE as
attrPosDef "0"  []         = C.AttrPos0
attrPosDef "1"  [a]        = C.AttrPos1  a
attrPosDef "2"  [a,b]      = C.AttrPos2  a b
attrPosDef "3"  [a,b,c]    = C.AttrPos3  a b c
attrPosDef "4"  [a,b,c,d]  = C.AttrPos4  a b c d
attrPosDef "1?" [a,b]      = C.AttrPos1Q a b
attrPosDef "V"  [a]        = C.AttrPosV  a
attrPosDef "1V" [a,b]      = C.AttrPos1V a b
attrPosDef _ xs            = attrBug $ unwords $ map C.attrNameText xs

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

