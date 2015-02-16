{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Builtin.Define
  ( RopDefine,
    ropList, def,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

-- | Constructor, usage, and attribute sorter
type RopDefine c = (C.RopCons c, C.RopUsage, C.AttrDefine)

-- | Make implementations of relmap operators.
ropList
    :: String         -- ^ Operator group
    -> [RopDefine c]  -- ^ Operator definitions
    -> [C.Rop c]      -- ^ Relmap operators
ropList group = map rop where
    rop (cons, usage, attr) =
        let name   = head $ words usage
            sorter = C.attrSort attr
        in C.Rop name group usage attr sorter cons

def :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
def cons usage attr = (cons, usage, attr') where
    n     = map attrName
    attr' = case map words $ B.divideBy (== '|') attr of
              [q : trunk]          -> attrDef q (n trunk) []
              [q : trunk, branch]  -> attrDef q (n trunk) (n branch)
              _                    -> ropBug attr

attrName :: String -> C.AttrName
attrName n@('-':_) | l == '/'    = C.AttrNameRelmap i
                   | l == '^'    = C.AttrNameLocal  i
                   | otherwise   = C.AttrNameNormal n
                   where l = last n
                         i = init n
attrName n = ropBug n

attrDef :: String -> [C.AttrName] -> [C.AttrName] -> C.AttrDefine
attrDef q ns = C.ropAttrDef $ select q ns where
    select "E"  as         = C.AttrPosE as
    select "0"  []         = C.AttrPos0
    select "1"  [a]        = C.AttrPos1  a
    select "2"  [a,b]      = C.AttrPos2  a b
    select "3"  [a,b,c]    = C.AttrPos3  a b c
    select "4"  [a,b,c,d]  = C.AttrPos4  a b c d
    select "1?" [a,b]      = C.AttrPos1Q a b
    select "V"  [a]        = C.AttrPosV  a
    select "1V" [a,b]      = C.AttrPos1V a b
    select "T1" [a,b]      = C.AttrPosT1 a b
    select "T2" [a,b,c]    = C.AttrPosT2 a b c
    select _ xs            = ropBug $ unwords $ map C.attrNameText xs

ropBug :: String -> a
ropBug x = B.bug $ "malformed attribute: " ++ x

