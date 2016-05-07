{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute sorter.

module Koshucode.Baala.Syntax.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Attr.Attr     as S
import qualified Koshucode.Baala.Syntax.Attr.AttrPos  as S

-- | Parse attribute sorter string.
--
-- >>> parseAttrLayout "0 | -c"
-- AttrLayout { positional = AttrPos0,
--                   named = [AttrNormal "c"] }
--
-- >>> parseAttrLayout "2 -a -b | -c"
-- AttrLayout { positional = AttrPos2 (AttrNormal "a") (AttrNormal "b"),
--                   named = [AttrNormal "c"] }

parseAttrLayout :: String -> S.AttrLayout
parseAttrLayout s = attr where
    n = map attrName
    attr = case map words $ B.divideBy (== '|') s of
             [q : trunk]          -> attrDef q (n trunk) []
             [q : trunk, branch]  -> attrDef q (n trunk) (n branch)
             _                    -> attrBug s

attrName :: String -> S.AttrName
attrName ('-' : n) | l == '^'    = attrLocal i
                   | l == '/'    = S.AttrRelmapNormal i  -- "-xxx/"
                   | otherwise   = S.AttrNormal       n  -- "-xxx"
                   where l = last n
                         i = init n
attrName n = attrBug n

attrLocal :: String -> S.AttrName
attrLocal n        | l == '/'    = S.AttrRelmapLocal i    -- "-xxx/^"
                   | otherwise   = S.AttrNormal      n    -- "-xxx^"
                   where l = last n
                         i = init n

attrDef :: String -> [S.AttrName] -> [S.AttrName] -> S.AttrLayout
attrDef q ns = S.attrLayout $ attrPosDef q ns

attrPosDef :: String -> [S.AttrName] -> S.AttrNamePos
attrPosDef "0"  []         = S.AttrPos0
attrPosDef "1"  [a]        = S.AttrPos1  a
attrPosDef "2"  [a,b]      = S.AttrPos2  a b
attrPosDef "3"  [a,b,c]    = S.AttrPos3  a b c
attrPosDef "4"  [a,b,c,d]  = S.AttrPos4  a b c d
attrPosDef "1?" [a,b]      = S.AttrPos1Q a b
attrPosDef "V"  [a]        = S.AttrPosV  a
attrPosDef "1V" [a,b]      = S.AttrPos1V a b
attrPosDef _ xs            = attrBug $ unwords $ map S.attrNameText xs

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

