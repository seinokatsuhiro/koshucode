{-# OPTIONS_GHC -Wall #-}

-- | Parser for attribute sorter.

module Koshucode.Baala.Syntax.Attr.Parse
  ( parseAttrLayout,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Para          as S
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
attrDef q ns branch = S.attrLayout spec branch where
    spec = S.paraSpec $ attrPosSpec q ns
           . S.paraReq [S.attrNameTrunk]
           . S.paraOpt branch

attrPosSpec :: String -> [S.AttrName] -> S.ParaSpecMap S.AttrName
attrPosSpec "0"  []            = S.para0
attrPosSpec "1"  ns@[_]        = S.paraItem     ns
attrPosSpec "2"  ns@[_,_]      = S.paraItem     ns
attrPosSpec "3"  ns@[_,_,_]    = S.paraItem     ns
attrPosSpec "4"  ns@[_,_,_,_]  = S.paraItem     ns
attrPosSpec "1?" [a,b]         = S.paraItemOpt  [a] b
attrPosSpec "V"  [a]           = S.paraItemRest [] a
attrPosSpec "1V" [a,b]         = S.paraItemRest [a] b
attrPosSpec _ xs               = attrBug $ unwords $ map S.attrNameText xs

attrBug :: String -> a
attrBug x = B.bug $ "malformed attribute: " ++ x

