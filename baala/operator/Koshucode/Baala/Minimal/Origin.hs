{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Minimal.Origin
( -- * source
  consSource,
  -- * id
  consId, relmapId,
  -- * empty
  consEmpty, relmapEmpty,
  -- * contents
  consContents, relmapContents,
  -- * reldee & reldum
  consReldee, consReldum,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop


-- ----------------------  source

consSource :: C.RopCons c
consSource use =
  do pattern  <- Rop.getWord  use "-pattern"
     terms    <- Rop.getTerms use "-term"
     Right $ C.relmapSource use pattern terms


-- ----------------------  id

consId :: C.RopCons c
consId use = Right $ relmapId use

{-| Identity mapping, i.e., do nothing. -}
relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapFlow use $ Right . C.relkitId


-- ----------------------  empty

consEmpty :: C.RopCons c
consEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapFlow use relkitEmpty

{-| Throw away all tuples in a relation. -}
relkitEmpty :: C.RelkitCalc c
relkitEmpty he1 = Right $ C.relkit he1 $ C.RelkitConst []


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents use =
    do n <- Rop.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: (Ord c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapContents use = C.relmapFlow use . relkitContents

relkitContents :: (Ord c) => B.Termname -> C.RelkitCalc c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = B.headFrom [n]
    kitf = map B.singleton . B.unique . concat


-- ----------------------  reldee & reldum

consReldee :: C.RopCons c
consReldee use = Right $ C.relmapConst use B.reldee

consReldum :: C.RopCons c
consReldum use = Right $ C.relmapConst use B.reldum

