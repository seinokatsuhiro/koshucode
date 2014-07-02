{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( ropsBuiltin,
  ropList,
  ropN, ropE, ropI, ropII, ropIJ, ropV, ropIV,

  -- * id
  consId, relmapId,
  -- $id
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C


-- | Built-in relmap operator.
--
--   [/r/ @|@ /s/]   Append relmaps
--
--   [@id@]          Identity relmap
--
ropsBuiltin :: [C.Rop c]
ropsBuiltin = ropList "builtin"
    --     CONSTRUCTOR  USAGE            ATTRIBUTE
    [ ropV consAppend   "append R ..."   "-relmap/"
    , ropN consId       "id"             ""
    ]

-- | Usage, constructor, and attribute sorter
type RopDef c = (String, C.RopCons c, C.RoaSpec)

-- | Make implementations of relation-mapping operators.
ropList
    :: String      -- ^ Operator group
    -> [RopDef c]  -- ^ List of operator definitions
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop :: RopDef c -> C.Rop c
    rop (usage, cons, roa) =
        let name   = head $ words usage
            sorter = C.roaSorter roa
        in C.Rop name group sorter cons usage

ropBase :: ([String] -> [String] -> C.RoaSpec) -> C.RopCons c -> String -> String -> RopDef c
ropBase a cons usage attr = (usage, cons, attr') where
    attr' = case B.divideBy (== '|') attr of
              [trunk]         -> a (names trunk) []
              [trunk, branch] -> a (names trunk) (names branch)
              _               -> ropBug attr
    names = map classify . words

    classify n@('-' : _) | last n == '/' = init n
                         | otherwise     = n
    classify n = ropBug n

ropBug :: String -> a
ropBug x = B.bug $ "malformed attribute: " ++ x

ropBugUnwords :: [String] -> a
ropBugUnwords = ropBug . unwords

ropN :: C.RopCons c -> String -> String -> RopDef c
ropN = ropBase a where
    a []       =  C.roaNone
    a xs       =  ropBugUnwords xs

ropE :: C.RopCons c -> String -> String -> RopDef c
ropE = ropBase a where
    a xs       =  C.roaEnum xs

ropI :: C.RopCons c -> String -> String -> RopDef c
ropI = ropBase a where
    a [x]      =  C.roaOne x
    a xs       =  ropBugUnwords xs

ropII :: C.RopCons c -> String -> String -> RopDef c
ropII = ropBase a where
    a [x1,x2]  =  C.roaTwo x1 x2
    a xs       =  ropBugUnwords xs

ropV :: C.RopCons c -> String -> String -> RopDef c
ropV = ropBase a where
    a [x1]     =  C.roaList x1
    a xs       =  ropBugUnwords xs

ropIV :: C.RopCons c -> String -> String -> RopDef c
ropIV = ropBase a where
    a [x1,x2]  =  C.roaOneList x1 x2
    a xs       =  ropBugUnwords xs

ropIJ :: C.RopCons c -> String -> String -> RopDef c
ropIJ = ropBase a where
    a [x1,x2] =  C.roaOneOpt x1 x2
    a xs      =  ropBugUnwords xs


-- ----------------------  append

consAppend :: C.RopCons c
consAppend = Right . foldl B.mappend B.mempty . C.ropSubrelmap


-- ----------------------  id

-- $id
--
--  Identity mapping, i.e., do nothing.
--
--    > pick /a /b | id

consId :: C.RopCons c
consId use = Right $ relmapId use

relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapFlow use $ Right . C.relkitId

