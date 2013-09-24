{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Minimal.Origin
( -- * source
  ropConsSource,
  -- * id
  ropConsId, relmapId,
  -- * empty
  ropConsEmpty, relmapEmpty,
  -- * contents
  ropConsContents, relmapContents,
  -- * reldee & reldum
  ropConsReldee, ropConsReldum,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  source

ropConsSource :: C.RopCons c
ropConsSource use =
  do sign <- Rop.getWord  use "-sign"
     ns   <- Rop.getTerms use "-term"
     Right $ C.relmapSource use sign ns


-- ----------------------  id

ropConsId :: C.RopCons c
ropConsId use = Right $ relmapId use

{-| Identity mapping, i.e., do nothing. -}
relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapCalc use "id" C.relfyId where



-- ----------------------  empty

ropConsEmpty :: C.RopCons c
ropConsEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapCalc use "empty" fy where
    fy _ = relfyEmpty

{-| Throw away all tuples in a relation. -}
relfyEmpty :: B.Relhead -> B.Ab (C.Relfy c)
relfyEmpty h1 = Right $ C.Relfy h1 (C.RelfyConst [])



-- ----------------------  contents

ropConsContents :: C.RopCons c
ropConsContents use =
    do n <- Rop.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: C.RopUse c -> B.Termname -> C.Relmap c
relmapContents use n = C.relmapCalc use "contents" fy where
    fy _ = relfyContents n

relfyContents :: B.Termname -> B.Relhead -> B.Ab (C.Relfy c)
relfyContents n _ = Right $ C.Relfy h2 (C.RelfyFull True f) where
    h2   = B.headFrom [n]
    f b1 = map B.singleton $ concat b1



-- ----------------------  reldee & reldum

ropConsReldee, ropConsReldum :: C.RopCons c
ropConsReldee use = Right $ C.relmapConst use "reldee" B.reldee
ropConsReldum use = Right $ C.relmapConst use "reldum" B.reldum


