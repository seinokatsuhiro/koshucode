{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute layout.
--   Syntax for layout is defined as the following __Layout__.
--   See 'parseAttrLayout' document for some examples.
--
--   [Layout]
--
--    * __Positional__           — Positional attributes
--    * __Positional @|@ Named__ — Positional and named attributes
--
--   [Positional]
--
--    * __@0@__                          — No positional attributes
--    * __@1@ Attr__                     — One positional attribute
--    * __@2@ Attr Attr__                — Two positional attributes
--    * __@3@ Attr Attr Attr__           — Three positional attributes
--    * __@4@ Attr Attr Attr Attr__      — Four positional attributes
--    * __@5@ Attr Attr Attr Attr Attr__ — Five positional attributes
--    * __@1?@ Attr Attr__               — One and optional positional attributes
--    * __@*@ Attr__                     — Variable-length positional attributes
--    * __@1*@ Attr Attr__               — One and variable-length positional attributes
--
--   [Named]
--
--    * __Attr ...__    — Named attributes
--
--   [Attr]
--
--    * __@-@Word__     — Normal attribute
--    * __@-@Word@/@__  — Relmap attribute
--    * __@-@Word@/^@__ — Local relmap attribute

module Koshucode.Baala.Syntax.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Para          as S
import qualified Koshucode.Baala.Syntax.Attr.Attr     as S
import qualified Koshucode.Baala.Syntax.Attr.AttrPos  as S

-- | Parse attribute layout.
--
-- No positional and one named @-c@ attribute.
--
-- >>> parseAttrLayout "0 | -c"
-- AttrLayout (ParaSpec { paraSpecOptN = [AttrNormal "@trunk", AttrNormal "c"], ... })
--
-- Two positional @-a@ @-b@ and one named @-c@ attributes.
--
-- >>> parseAttrLayout "2 -a -b/ | -c"
-- AttrLayout (ParaSpec { paraSpecPos  = ParaItem 2 [AttrNormal "a", AttrRelmapNormal "b"]
--                      , paraSpecReqP = [AttrNormal "a", AttrRelmapNormal "b"]
--                      , paraSpecOptN = [AttrNormal "@trunk", AttrNormal "c"], ... })

parseAttrLayout :: String -> S.AttrLayout
parseAttrLayout s = lay where
    n = map attrName
    lay = case map words $ B.divideBy (== '|') s of
            [q : pos]         -> attrLayout q (n pos) []
            [q : pos, named]  -> attrLayout q (n pos) (n named)
            _                 -> attrBug s

attrName :: String -> S.AttrName
attrName ('-' : n) | l == '^'    = attrLocal i
                   | l == '/'    = S.AttrRelmapNormal i  -- "-xxx/"
                   | otherwise   = S.AttrNormal       n  -- "-xxx"
                   where l = last n
                         i = init n
attrName n = attrBug n

attrLocal :: String -> S.AttrName
attrLocal n | l == '/'    = S.AttrRelmapLocal i    -- "-xxx/^"
            | otherwise   = S.AttrNormal      n    -- "-xxx^"
            where l = last n
                  i = init n

attrLayout :: String -> [S.AttrName] -> [S.AttrName] -> S.AttrLayout
attrLayout q nsP nsN = S.attrLayout $ attrSpec q nsP nsN

attrSpec :: String -> [S.AttrName] -> [S.AttrName] -> S.AttrParaSpec
attrSpec q nsP nsN = S.paraSpec $ pos . opt where
    pos = attrSpecPos q nsP
    opt = S.paraOpt $ S.attrNameTrunk : nsN

attrSpecPos :: String -> [S.AttrName] -> S.ParaSpecMap S.AttrName
attrSpecPos "0"  ns     = attrSpecPosN 0 ns
attrSpecPos "1"  ns     = attrSpecPosN 1 ns
attrSpecPos "2"  ns     = attrSpecPosN 2 ns
attrSpecPos "3"  ns     = attrSpecPosN 3 ns
attrSpecPos "4"  ns     = attrSpecPosN 4 ns
attrSpecPos "5"  ns     = attrSpecPosN 5 ns
attrSpecPos "1?" [a,b]  = S.paraItemOpt  [a] b
attrSpecPos "V"  [a]    = S.paraItemRest [] a
attrSpecPos "1V" [a,b]  = S.paraItemRest [a] b
attrSpecPos _ xs        = attrBug $ unwords $ map S.attrNameText xs

attrSpecPosN :: Int -> [S.AttrName] -> S.ParaSpecMap S.AttrName
attrSpecPosN l ns
    | length ns == l  = S.paraItem ns
    | otherwise       = attrBug $ unwords $ map S.attrNameText ns

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

