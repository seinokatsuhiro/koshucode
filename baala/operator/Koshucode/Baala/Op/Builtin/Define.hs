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
    rop :: RopDefine c -> C.Rop c
    rop (cons, usage, roa) =
        let name   = head $ words usage
            sorter = C.attrSort roa
        in C.Rop name group sorter cons usage

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
attrDef "E"  as       = C.roaEnum as
attrDef "0"  []       = C.roaNone
attrDef "1"  [a]      = C.roaOne a
attrDef "2"  [a,b]    = C.roaTwo a b
attrDef "3"  [a,b,c]  = C.roaThree a b c
attrDef "1?" [a,b]    = C.roaOneOpt a b
attrDef "V"  [a]      = C.roaList a
attrDef "1V" [a,b]    = C.roaOneList a b
attrDef "T1" [a,b]    = C.roaTermsOne a b
attrDef "T2" [a,b,c]  = C.roaTermsTwo a b c
attrDef _ xs          = ropBug $ unwords $ map C.attrNameText xs

ropBug :: String -> a
ropBug x = B.bug $ "malformed attribute: " ++ x

