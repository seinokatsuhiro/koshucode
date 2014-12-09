{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Builtin.Define
  ( RopDefine,
    ropList,
    ropN, ropE,
    ropI, ropII, ropIJ, ropIII,
    ropV, ropIV,
    ropTI, ropTII,
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

ropBy :: ([C.AttrName] -> [C.AttrName] -> C.AttrDefine)
        -> C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropBy a cons usage attr = (cons, usage, attr') where
    attr' = case B.divideBy (== '|') attr of
              [trunk]          -> a (names trunk) []
              [trunk, branch]  -> a (names trunk) (names branch)
              _                -> ropBug attr
    names = map classify . words

    classify "-<"                     = C.AttrNameNest     "-<"
    classify n@('-' : _) | l == '/'   = C.AttrNameRelmap $ init n
                         | l == '<'   = C.AttrNameNest   $ init n
                         | otherwise  = C.AttrNameNormal   n
                         where l = last n
    classify n = ropBug n

ropBug :: String -> a
ropBug x = B.bug $ "malformed attribute: " ++ x

ropBugUnwords :: [C.AttrName] -> a
ropBugUnwords = ropBug . unwords . map C.attrNameText

-- | No attributes
ropN :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropN = ropBy a where
    a []         = C.roaNone
    a xs         = ropBugUnwords xs

-- | Enumerating attributes
ropE :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropE = ropBy a where
    a xs         = C.roaEnum xs

-- | One attribute
ropI :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropI = ropBy a where
    a [x]        = C.roaOne x
    a xs         = ropBugUnwords xs

-- | Two attributes
ropII :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropII = ropBy a where
    a [x1,x2]    = C.roaTwo x1 x2
    a xs         = ropBugUnwords xs

-- | Three attributes
ropIII :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropIII = ropBy a where
    a [x1,x2,x3] = C.roaThree x1 x2 x3
    a xs         = ropBugUnwords xs

-- | One and optional attributes
ropIJ :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropIJ = ropBy a where
    a [x1,x2]    = C.roaOneOpt x1 x2
    a xs         = ropBugUnwords xs

-- | Variable-length attributes
ropV :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropV = ropBy a where
    a [x1]       = C.roaList x1
    a xs         = ropBugUnwords xs

-- | One and variable-length attributes
ropIV :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropIV = ropBy a where
    a [x1,x2]    = C.roaOneList x1 x2
    a xs         = ropBugUnwords xs

-- | Term list and one attribute
ropTI :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropTI = ropBy a where
    a [x1,x2]    = C.roaTermsOne x1 x2
    a xs         = ropBugUnwords xs

-- | Term list and two attributes
ropTII :: C.RopCons c -> C.RopUsage -> String -> RopDefine c
ropTII = ropBy a where
    a [x1,x2,x3] = C.roaTermsTwo x1 x2 x3
    a xs         = ropBugUnwords xs

