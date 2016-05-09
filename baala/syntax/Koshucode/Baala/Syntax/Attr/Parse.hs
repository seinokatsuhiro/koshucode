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
    parseAttrSpec,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Para          as S
import qualified Koshucode.Baala.Syntax.Attr.Attr     as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName as S


-- ----------------------  AttrName-based

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
parseAttrLayout = S.AttrLayout . map (fmap branch) . parseAttrSpec

branch :: S.ParaSpec String -> S.AttrBranch
branch = S.attrBranch . fmap attrName

attrName :: String -> S.AttrName
attrName = name . reverse where
    name ('^' : '/' : n) = S.AttrRelmapLocal  $ reverse n
    name ('/' : n)       = S.AttrRelmapNormal $ reverse n
    name n               = S.AttrNormal       $ reverse n


-- ----------------------  String-based

parseAttrSpec :: String -> [(Maybe String, S.ParaSpec String)]
parseAttrSpec = multi where
    divide c  = B.divideBy (== c)
    multi     = map single . divide '|'

    single s  = case divide ':' s of
                  [n, l]   -> (Just $ B.trimBoth n, layout l)
                  [l]      -> (Nothing, layout l)
                  _        -> attrBug s

    layout l  = case map words $ divide '.' l of
                  [q : ps]      -> attrSpec q ps []
                  [q : ps, ns]  -> attrSpec q ps ns
                  _             -> attrBug l

attrSpec :: String -> [String] -> [String] -> S.ParaSpec String
attrSpec q nsP nsN = S.paraSpec $ pos . req . opt where
    pos  = attrSpecPos q $ map snd $ map attrString nsP
    req  = S.paraReq $ ns False
    opt  = S.paraOpt $ "@trunk" : ns True
    ns b = map snd $ filter (\n -> fst n == b) $ fmap attrString nsN

attrString :: String -> (Bool, String)
attrString = hyph where
    hyph ('-' : n)  = opt n
    hyph n          = attrBug n

    opt n  | l == '?'    = (True,  i)
           | otherwise   = (False, n)
           where (l, i)  = lastInit n

lastInit :: [a] -> (a, [a])
lastInit n = (last n, init n)

attrSpecPos :: String -> [String] -> S.ParaSpecMap String
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
attrSpecPos _ ns        = attrBug $ unwords ns

attrSpecPosN :: Int -> [String] -> S.ParaSpecMap String
attrSpecPosN l ns
    | length ns == l  = S.paraItem ns
    | otherwise       = attrBug $ unwords ns

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

