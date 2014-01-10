{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Global
( 
  -- * koshu-cop
  ropConsKoshuCop,
  -- * koshu-rop
  ropConsKoshuRop,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop
import qualified Koshucode.Baala.Vanilla.Type  as Rop



-- ----------------------  koshu-cop

ropConsKoshuCop :: Rop.VRopCons
ropConsKoshuCop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuCop use name = C.relmapGlobal use $ relfyKoshuCop name

relfyKoshuCop :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuCop name C.Global { C.globalCops = cops } _ = r2 where
    r2 = Right $ C.relfy h2 $ C.RelfyConst names
    h2 = B.headFrom [name]
    names = map (B.singleton . C.putText . B.name) cops


-- ----------------------  koshu-rop

ropConsKoshuRop :: Rop.VRopCons
ropConsKoshuRop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuRop use name

relmapKoshuRop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuRop use name = C.relmapGlobal use $ relfyKoshuRop name

relfyKoshuRop :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuRop name C.Global { C.globalRops = rops } _ = r2 where
    r2 = Right $ C.relfy h2 $ C.RelfyConst names
    h2 = B.headFrom [name]
    names = map (B.singleton . C.putText . C.ropName) rops

