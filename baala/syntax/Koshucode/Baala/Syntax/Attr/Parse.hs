{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute layout.
--   Syntax for layout is defined as the following __Multiple__.
--   See 'parseAttrLayout' document for some examples.
--
--   [Multiple]
--
--    * __Single__                       — Single layout
--    * __Single @|@ Multiple__ ...      — Multiple layout
--
--   [Single]
--
--    * __Layout__                       — Layout without tag
--    * __Word @:@ Layout__              — Layout with tag
--
--   [Layout]
--
--    * __Positional__                   — Positional attributes
--    * __Positional @.@ Named__         — Positional and named attributes
--
--   [Positional]
--
--    * __@0@__                          — No positional attributes
--    * __@1@ Attr__                     — One positional attribute
--    * __@2@ Attr Attr__                — Two positional attributes
--    * __@3@ Attr Attr Attr__           — Three positional attributes
--    * __@4@ Attr Attr Attr Attr__      — Four positional attributes
--    * __@5@ Attr Attr Attr Attr Attr__ — Five positional attributes
--    * __@?@ Attr__                     — Optional positional attribute
--    * __@1?@ Attr Attr__               — One and optional positional attributes
--    * __@*@ Attr__                     — Rest positional attribute
--    * __@1*@ Attr Attr__               — One and rest positional attributes
--
--   [Named]
--
--    * __Attr ...__           — Named attributes
--
--   [Attr]
--
--    * __@-@Word Opt__        — Normal attribute
--    * __@-@Word@/@ Opt__     — Relmap attribute
--    * __@-@Word@/^@ Opt__    — Local relmap attribute
--
--   [Opt]
--
--    * __@?@__                — Optional attribute
--    * __Empty__              — Required attribute

module Koshucode.Baala.Syntax.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Para          as S
import qualified Koshucode.Baala.Syntax.Attr.Attr     as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName as S

-- | Parse attribute layout.
--
-- No positional and one named @-c@ attribute.
--
-- >>> parseAttrLayout "0 . -c"
-- [ (Nothing, AttrLayout (ParaSpec {
--                paraSpecReqN = [AttrNormal "c"]
--              , paraSpecOptN = [AttrNormal "@trunk"], ... })) ]
--
-- Two positional @-a@ @-b@ and one named @-c@ attributes.
--
-- >>> parseAttrLayout "2 -a -b/ . -c"
-- [ (Nothing, AttrLayout (ParaSpec {
--                paraSpecPos  = ParaItem 2 [AttrNormal "a", AttrRelmapNormal "b"]
--               , paraSpecReqP = [AttrNormal "a", AttrRelmapNormal "b"]
--               , paraSpecOptN = [AttrNormal "@trunk", AttrNormal "c"], ... })) ]

parseAttrLayout :: String -> S.AttrLayout
parseAttrLayout = alt where
    alt s     = S.AttrLayout $ map nameLay $ B.divideBy (== '|') s

    nameLay s = case B.divideBy (== ':') s of
                  [n, s']  -> (Just $ B.trimBoth n, lay s')
                  [s']     -> (Nothing, lay s')
                  _        -> attrBug s

    lay s     = case map words $ B.divideBy (== '.') s of
                  [q : pos]         -> branch q (name pos) []
                  [q : pos, named]  -> branch q (name pos) (name named)
                  _                 -> attrBug s

    name = map attrName

type BoolName = (Bool, S.AttrName)

attrName :: String -> BoolName
attrName = hyph where

    hyph ('-' : n)  = opt n
    hyph n          = attrBug n

    opt n     | l == '?'    = name True  i
              | otherwise   = name False n
              where (l, i)  = lastInit n

    name o n  | l == '^'    = local o i
              | l == '/'    = (o, S.AttrRelmapNormal i)  -- "-xxx/"
              | otherwise   = (o, S.AttrNormal       n)  -- "-xxx"
              where (l, i)  = lastInit n

    local o n | l == '/'    = (o, S.AttrRelmapLocal i)    -- "-xxx/^"
              | otherwise   = (o, S.AttrNormal      n)    -- "-xxx^"
              where (l, i)  = lastInit n

    lastInit n = (last n, init n)

branch :: String -> [BoolName] -> [BoolName] -> S.AttrBranch
branch q nsP nsN = S.attrBranch $ attrSpec q nsP nsN

attrSpec :: String -> [BoolName] -> [BoolName] -> S.AttrParaSpec
attrSpec q nsP nsN = S.paraSpec $ pos . req . opt where
    pos  = attrSpecPos q $ map snd nsP
    req  = S.paraReq $ ns False
    opt  = S.paraOpt $ S.attrNameTrunk : ns True
    ns b = map snd $ filter (\n -> fst n == b) nsN

attrSpecPos :: String -> [S.AttrName] -> S.ParaSpecMap S.AttrName
attrSpecPos "0"  ns     = attrSpecPosN 0 ns
attrSpecPos "1"  ns     = attrSpecPosN 1 ns
attrSpecPos "2"  ns     = attrSpecPosN 2 ns
attrSpecPos "3"  ns     = attrSpecPosN 3 ns
attrSpecPos "4"  ns     = attrSpecPosN 4 ns
attrSpecPos "5"  ns     = attrSpecPosN 5 ns
attrSpecPos "?"  [a]    = S.paraItemOpt  [] a
attrSpecPos "1?" [a,b]  = S.paraItemOpt  [a] b
attrSpecPos "*"  [a]    = S.paraItemRest [] a
attrSpecPos "1*" [a,b]  = S.paraItemRest [a] b
attrSpecPos _ xs        = attrBug $ unwords $ map S.attrNameText xs

attrSpecPosN :: Int -> [S.AttrName] -> S.ParaSpecMap S.AttrName
attrSpecPosN l ns
    | length ns == l  = S.paraItem ns
    | otherwise       = attrBug $ unwords $ map S.attrNameText ns

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

