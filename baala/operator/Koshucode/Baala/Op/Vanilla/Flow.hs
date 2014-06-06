{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Flow
( 
  -- * const
  consConst, relmapConst, relkitConst,
  -- $const

  -- * size
  consSize, relmapSize, relkitSize,
  -- $size
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op
import qualified Koshucode.Baala.Op.Message   as Message



-- ----------------------  const

-- $const
--
--  Same as relmap @dee@
--  
--    > const {| | |}
--
--  Same as relmap @dum@
--  
--    > const {| |}

consConst :: (C.CContent c) => C.RopCons c
consConst use =
    do tree <- Op.getTree use "-lit"
       lit  <- C.litContent tree
       case C.isRel lit of
         True  -> Right $ relmapConst use $ C.gRel lit
         False -> Message.reqRel

relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst use = C.relmapFlow use . relkitConst

relkitConst :: B.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (B.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo



-- ----------------------  size

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c

consSize :: (C.CDec c) => C.RopCons c
consSize use =
  do n <- Op.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapSize use n = C.relmapFlow use $ relkitSize n

relkitSize :: (C.CDec c) => B.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pDecFromInt $ length bo1 ]]

