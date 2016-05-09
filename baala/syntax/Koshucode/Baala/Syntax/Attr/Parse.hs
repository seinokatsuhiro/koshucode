{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute layout.
--   Syntax for layout is defined as the following __Multiple__.
--   See 'parseAttrLayout' document for some examples.
--
--   [Multiple]
--
--    * __Single__                    — Single layout
--    * __Single @|@ Multiple__ ...   — Multiple layout
--
--   [Single]
--
--    * __Layout__                    — Layout without tag
--    * __Word @:@ Layout__           — Layout with tag
--
--   [Layout]
--
--    * __Positional__                — Positional attributes
--    * __Positional @.@ Named__      — Positional and named attributes
--
--   [Positional]
--
--    * __PosAttr ...__               — Positional attributes
--
--   [Named]
--
--    * __NamedAttr ...__             — Named attributes
--
--   [PosAttr]
--
--    * __Word__               — Required attribute
--    * __Word@?@__            — Optional attribute
--    * __Word@*@__            — Rest attribute
--
--   [NamedAttr]
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
                  [ps]      -> attrSpec ps []
                  [ps, ns]  -> attrSpec ps ns
                  _         -> attrBug l

attrSpec :: [String] -> [String] -> S.ParaSpec String
attrSpec nsP nsN = S.paraSpec $ pos . req . opt where
    pos  = attrSpecPos nsP
    req  = S.paraReq $ ns False
    opt  = S.paraOpt $ "@trunk" : ns True
    ns b = map snd $ filter (\n -> fst n == b) $ fmap attrString nsN

attrString :: String -> (Bool, String)
attrString = hyph where
    hyph ('-' : n)  = opt n
    hyph n          = attrBug n

    opt n  | l == '?'    = (True,  i)
           | otherwise   = (False, n)
           where (i, l)  = initLast n

attrSpecPos :: [String] -> S.ParaSpecMap String
attrSpecPos ns =
    case ror . unhyphen <$> ns of
      rs | all isReq rs  -> attrSpecPosN (length rs) (unror <$> rs)
      rs -> case initLast rs of
              (i, Opt r)  | all isReq i -> S.paraItemOpt  (unror <$> i) r
              (i, Rest r) | all isReq i -> S.paraItemRest (unror <$> i) r
              _                         -> attrBug $ unwords ns

attrSpecPosN :: Int -> [String] -> S.ParaSpecMap String
attrSpecPosN l ns
    | length ns == l  = S.paraItem ns
    | otherwise       = attrBug $ unwords ns

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x


-- ----------------------  ROR

data Ror = Req String | Opt String | Rest String
           deriving (Show, Eq, Ord)

isReq :: Ror -> Bool
isReq (Req _) = True
isReq _       = False

ror :: String -> Ror
ror n = case initLast n of
          (n', '?') -> Opt  n'
          (n', '*') -> Rest n'
          _         -> Req  n

unhyphen :: String -> String
unhyphen ('-' : n) = n
unhyphen n         = attrBug n

unror :: Ror -> String
unror (Req  n) = n
unror (Opt  n) = n
unror (Rest n) = n

initLast :: [a] -> ([a], a)
initLast n = (init n, last n)

