{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Global
( 
  -- * koshu-cop
  ropConsKoshuCop,
  -- * koshu-cop-infix
  ropConsKoshuCopInfix,
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
relfyKoshuCop name C.Global { C.globalCops = (cops, _) } _ = r2 where
    r2 = Right $ C.relfy h2 $ C.RelfyConst names
    h2 = B.headFrom [name]
    names = map (B.singleton . C.putText . B.name) cops


-- ----------------------  koshu-cop-infix

ropConsKoshuCopInfix :: Rop.VRopCons
ropConsKoshuCopInfix use =
  do name   <- Rop.getTerm use "-name"
     height <- Rop.getMaybe Rop.getTerm use "-height"
     dir    <- Rop.getMaybe Rop.getTerm use "-dir"
     Right $ relmapKoshuCopInfix use (name, height, dir)

relmapKoshuCopInfix :: (C.CContent c) => C.RopUse c -> (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.Relmap c
relmapKoshuCopInfix use terms = C.relmapGlobal use $ relfyKoshuCopInfix terms

relfyKoshuCopInfix :: (C.CContent c) => (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuCopInfix (name, height, dir) C.Global { C.globalCops = (_, htab) } _ = r2 where
    r2 = Right $ C.relfy h2 $ C.RelfyConst (map put htab)
    h2 = B.headFrom $   [name] ++ heightMaybe B.singleton     ++ dirMaybe B.singleton
    put (n,ih) = [C.putText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = B.maybeEmpty height
    dirMaybe    = B.maybeEmpty dir

    heightTerm (Left  h) _ = [C.putDecFromInt h]
    heightTerm (Right h) _ = [C.putDecFromInt h]

    dirTerm    (Left  _) _ = [C.putText "left"]
    dirTerm    (Right _) _ = [C.putText "right"]


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

