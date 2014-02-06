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
relmapId use = C.relmapCalc use $ Right . C.relkitId


-- ----------------------  empty

consEmpty :: C.RopCons c
consEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapCalc use relkitEmpty

{-| Throw away all tuples in a relation. -}
relkitEmpty :: C.RelkitCalc c
relkitEmpty h1 = Right $ C.relkit h1 (C.RelkitConst [])


-- ----------------------  contents

consContents :: C.RopCons c
consContents use =
    do n <- Rop.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: C.RopUse c -> B.Termname -> C.Relmap c
relmapContents use = C.relmapCalc use . relkitContents

relkitContents :: B.Termname -> C.RelkitCalc c
relkitContents n _ = Right $ C.relkit h2 (C.RelkitFull True f) where
    h2   = B.headFrom [n]
    f b1 = map B.singleton $ concat b1


-- ----------------------  reldee & reldum

consReldee :: C.RopCons c
consReldee use = Right $ C.relmapConst use B.reldee

consReldum :: C.RopCons c
consReldum use = Right $ C.relmapConst use B.reldum

