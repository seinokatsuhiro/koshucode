{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute layout.

module Koshucode.Baala.Syntax.Attr.Parse
  ( parseAttrLayout,
    parseAttrLayoutL,
    parseParaSpec,
    parseParaSpecs,
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Para          as S
import qualified Koshucode.Baala.Syntax.Attr.Attr     as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName as S


-- ----------------------  For relmap attribute

-- | Parse relmap attribute layout.
--
--   * __@-@Word__        — Normal attribute
--   * __@-@Word@/@__     — Relmap attribute
--   * __@-@Word@/^@__    — Local relmap attribute
--
--   >>> parseAttrLayout "-a"
--   AttrLayout [(Nothing, AttrBranch (
--     ParaSpec { paraSpecPos = ParaItem 1 [AttrNormal "a"]
--              , paraSpecReqP = [AttrNormal "a"]
--              , paraSpecOptP = [], paraSpecReqN = []
--              , paraSpecOptN = [AttrNormal "@trunk"]
--              , paraSpecFirst = [], paraSpecLast = [], paraSpecMulti = [] }
--              ))]
--
parseAttrLayout :: String -> S.AttrLayout
parseAttrLayout = parseAttrLayoutL . divide '|'

parseAttrLayoutL :: [String] -> S.AttrLayout
parseAttrLayoutL = S.AttrLayout . map (fmap branch) . parseParaSpecs

branch :: S.ParaSpec String -> S.AttrBranch
branch = S.attrBranch . trunk . fmap attrName where
    trunk spec = spec { S.paraSpecOptN = S.attrNameTrunk : S.paraSpecOptN spec }

attrName :: String -> S.AttrName
attrName = name . reverse . unhyphen where
    name ('^' : '/' : n) = S.AttrRelmapLocal  $ reverse n
    name ('/' : n)       = S.AttrRelmapNormal $ reverse n
    name n               = S.AttrNormal       $ reverse n

unhyphen :: O.StringMap
unhyphen ('-' : n) = n
unhyphen n         = paraBug "no hyphen" n


-- ----------------------  For string parameter

-- | Parse parameter layout.
--
-- No positional and one named required @c@ attribute.
--
--  >>> parseParaSpec ". c"
--  [(Nothing, ParaSpec { paraSpecReqN = ["c"], ... })]
--
-- Positional required @a@, rest @b@, and one named @c@ attributes.
--
--  >>> parseParaSpec "a b* . c"
--  [(Nothing, ParaSpec { paraSpecPos = ParaItemRest 1 ["a"] "b"
--                      , paraSpecReqP = ["a"]
--                      , paraSpecOptP = ["b"]
--                      , paraSpecReqN = ["c"], ... })]
--
-- Set of named layouts.
--
--  >>> parseParaSpec "a : a | ab : a b"
--  [ (Just "a",  ParaSpec { paraSpecPos = ParaItem 1 ["a"], paraSpecReqP = ["a"], ... })
--  , (Just "ab", ParaSpec { paraSpecPos = ParaItem 2 ["a","b"], paraSpecReqP = ["a","b"], ... })]
--
-- Syntax for parameter layout.
--
--  [Multiple]
--
--    * __Single__                    — Single layout
--    * __Single @|@ Multiple__ ...   — Multiple layout
--
--  [Single]
--
--    * __Layout__                    — Layout without tag
--    * __Word @:@ Layout__           — Layout with tag
--
--  [Layout]
--
--    * __Positional__                — Positional parameters
--    * __Positional @.@ Named__      — Positional and named parameters
--
--  [Positional]
--
--    * __Word ...__                  — Positional required parameters
--    * __Word ... Word@?@ ...__      — Positional required and optional parameters
--    * __Word ... Word@*@__          — Positional required and rest parameters
--
--  [Named]
--
--    * __Name ...__                  — Named parameters
--
--  [Name]
--
--    * __Word Opt__                  — Named parameter
--
--  [Opt]
--
--    * __Empty__                     — Required parameter
--    * __@?@__                       — Optional parameter
--
parseParaSpec :: String -> [(Maybe S.ParaTag, S.ParaSpec String)]
parseParaSpec = parseParaSpecs . divide '|'

-- | Parse multiple specifications.
parseParaSpecs :: [String] -> [(Maybe S.ParaTag, S.ParaSpec String)]
parseParaSpecs = map parseParaSpec1

-- | Parse single specification.
parseParaSpec1 :: String -> (Maybe S.ParaTag, S.ParaSpec String)
parseParaSpec1 = single where
    single s  = case divide ':' s of
                  [n, l]   -> (Just $ O.trimBoth n, layout l)
                  [l]      -> (Nothing, layout l)
                  _        -> paraBug "neither T:L/L" s

    layout l  = case map words $ divide '.' l of
                  [ps]      -> paraSpec ps []
                  [ps, ns]  -> paraSpec ps ns
                  _         -> paraBug "neither P/P.L" l

divide :: (Eq a) => a -> [a] -> [[a]]
divide c = B.divideBy (== c)

paraSpec :: [String] -> [String] -> S.ParaSpec String
paraSpec nsP nsN = S.paraSpec $ pos . req . opt where
    pos  = paraSpecPos nsP
    req  = S.paraReq $ ns False
    opt  = S.paraOpt $ ns True
    ns b = map snd $ filter (\n -> fst n == b) $ fmap paraString nsN

paraString :: String -> (Bool, String)
paraString n
    | l == '?'    = (True,  i)
    | otherwise   = (False, n)
    where (i, l)  = initLast n

paraSpecPos :: [String] -> S.ParaSpecMap String
paraSpecPos ns =
    case span isReq $ ror <$> ns of
      (rs, [])                -> S.paraItem (unror <$> rs)
      (rs, [Rest x])          -> S.paraItemRest (unror <$> rs) x
      (rs, xs) | all isOpt xs -> S.paraItemOpt  (unror <$> rs) (unror <$> xs)
               | otherwise    -> paraBug "req/opt/rest" $ unwords ns

paraBug :: String -> String -> a
paraBug label x = B.bug $ "malformed layout (" ++ label ++ "): " ++ x


-- ----------------------  ROR

data Ror = Req String | Opt String | Rest String
           deriving (Show, Eq, Ord)

ror :: String -> Ror
ror n = case initLast n of
          (n', '?') -> Opt  n'
          (n', '*') -> Rest n'
          _         -> Req  n

unror :: Ror -> String
unror (Req  n) = n
unror (Opt  n) = n
unror (Rest n) = n

isReq :: Ror -> Bool
isReq (Req _) = True
isReq _       = False

isOpt :: Ror -> Bool
isOpt (Opt _) = True
isOpt _       = False

initLast :: [a] -> ([a], a)
initLast n = (init n, last n)

