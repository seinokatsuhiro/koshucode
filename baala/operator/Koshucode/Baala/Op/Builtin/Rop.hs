{-# OPTIONS_GHC -Wall #-}

-- | Built-in relmap operator.

module Koshucode.Baala.Op.Builtin.Rop
( ropsBuiltin,
  ropList,
  ropN, ropI, ropII, ropV, ropIV,

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
    [ ropV consAppend  "append R ..."   "-relmap"
    , ropN consId      "id"
    ]


-- | Make implementations of relation-mapping operators.
ropList
    :: String      -- ^ Operator group
    -> [(String, C.RopCons c, C.RoaSpec)]
                   -- ^ Synopsis, constructor, and attribute sorter
    -> [C.Rop c]   -- ^ List of relation-mapping operators
ropList group = map rop where
    rop :: (String, C.RopCons c, C.RoaSpec) -> C.Rop c
    rop (usage, cons, roa) =
        let name   = head $ words usage
            sorter = C.roaSorter roa
        in C.Rop name group sorter cons usage

ropBase :: ([String] -> [String] -> C.RoaSpec) -> C.RopCons c -> String -> String -> (String, C.RopCons c, C.RoaSpec)
ropBase a cons usage attr = (usage, cons, attr') where
    attr' = case B.divideBy (== '|') attr of
              [trunk]         -> a (words trunk) []
              [trunk, branch] -> a (words trunk) (words branch)
              _               -> B.bug $ "malformed attribute: " ++ attr

ropN :: C.RopCons c -> String -> (String, C.RopCons c, C.RoaSpec)
ropN cons usage = ropBase a cons usage "" where
    a []      =  C.roaNone
    a _       =  B.bug "ropI"

ropI :: C.RopCons c -> String -> String -> (String, C.RopCons c, C.RoaSpec)
ropI = ropBase a where
    a [x]     =  C.roaOne x
    a _       =  B.bug "ropI"

ropII :: C.RopCons c -> String -> String -> (String, C.RopCons c, C.RoaSpec)
ropII = ropBase a where
    a [x1,x2] =  C.roaTwo x1 x2
    a _       =  B.bug "ropI"

ropV :: C.RopCons c -> String -> String -> (String, C.RopCons c, C.RoaSpec)
ropV = ropBase a where
    a [x1]    =  C.roaList x1
    a _       =  B.bug "ropI"

ropIV :: C.RopCons c -> String -> String -> (String, C.RopCons c, C.RoaSpec)
ropIV = ropBase a where
    a [x1,x2] =  C.roaOneList x1 x2
    a _       =  B.bug "ropI"


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

