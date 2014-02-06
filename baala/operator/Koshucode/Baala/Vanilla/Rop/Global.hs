{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Global
( 
  -- * koshu-cop
  consKoshuCop, relkitKoshuCop,
  -- * koshu-cop-infix
  consKoshuCopInfix, relkitKoshuCopInfix,
  -- * koshu-rop
  consKoshuRop, relkitKoshuRop,
  -- * koshu-version
  consKoshuVersion, relkitKoshuVersion,
) where

import qualified Data.Version                  as V
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop



-- ----------------------  koshu-cop

consKoshuCop :: C.CContent c => C.RopCons c
consKoshuCop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuCop use = C.relmapGlobal use . relkitKoshuCop

relkitKoshuCop :: (C.CContent c) => B.Termname -> C.RelkitGlobal c
relkitKoshuCop name C.Global { C.globalCops = (cops, _) } _ =
    Right $ C.relkitConstBody [name] $ map (B.singleton . C.putText . B.name) cops



-- ----------------------  koshu-cop-infix

consKoshuCopInfix :: (C.CContent c) => C.RopCons c
consKoshuCopInfix use =
  do name   <- Rop.getTerm use "-name"
     height <- Rop.getMaybe Rop.getTerm use "-height"
     dir    <- Rop.getMaybe Rop.getTerm use "-dir"
     Right $ relmapKoshuCopInfix use (name, height, dir)

relmapKoshuCopInfix :: (C.CContent c) => C.RopUse c -> (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.Relmap c
relmapKoshuCopInfix use = C.relmapGlobal use . relkitKoshuCopInfix

relkitKoshuCopInfix :: (C.CContent c) => (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.RelkitGlobal c
relkitKoshuCopInfix (name, height, dir) C.Global { C.globalCops = (_, htab) } _ = r2 where
    r2 = Right $ C.relkit h2 $ C.RelkitConst (map put htab)
    h2 = B.headFrom $   [name] ++ heightMaybe B.singleton     ++ dirMaybe B.singleton
    put (n,ih) = [C.putText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = B.maybeEmpty height
    dirMaybe    = B.maybeEmpty dir

    heightTerm (Left  h) _ = [C.putDecFromInt h]
    heightTerm (Right h) _ = [C.putDecFromInt h]

    dirTerm    (Left  _) _ = [C.putText "left"]
    dirTerm    (Right _) _ = [C.putText "right"]


-- ----------------------  koshu-rop

consKoshuRop :: (C.CContent c) => C.RopCons c
consKoshuRop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuRop use name

relmapKoshuRop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuRop use = C.relmapGlobal use . relkitKoshuRop

relkitKoshuRop :: (C.CContent c) => B.Termname -> C.RelkitGlobal c
relkitKoshuRop name C.Global { C.globalRops = rops } _ =
    Right $C.relkitConstBody [name] $ map (B.singleton . C.putText . C.ropName) rops

-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

consKoshuVersion :: (C.CContent c) => C.RopCons c
consKoshuVersion use =
  do n   <- Rop.getTerm  use "-term"
     ver <- Rop.getTrees use "-version"
     case ver of
       []      -> Right $ C.relmapGlobal use $ relkitKoshuVersion n
       [f]     -> check n f f
       [f, t]  -> check n f t
       _       -> Left $ B.abortOperand ""
  where
    check n f t = do
      from <- C.litContent f
      to   <- C.litContent t
      Right $ C.relmapGlobal use $ relkitKoshuVersionCheck (from, to) n

relkitKoshuVersion :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relkit c)
relkitKoshuVersion n C.Global { C.globalVersion = ver } _ =
    Right $ C.relkitConstSingleton [n] [ C.putList $ map C.putDecFromInt $ apiVersion ver ]

relkitKoshuVersionCheck :: (C.CContent c) => (c, c) -> B.Termname -> C.RelkitGlobal c
relkitKoshuVersionCheck (from, to) n C.Global { C.globalVersion = ver } _
    | version >= from && version <= to  = Right $ C.relkitConstSingleton [n] [version] -- todo
    | otherwise = Right $ C.relkitConstEmpty [n]
    where version = C.putList $ map C.putDecFromInt $ apiVersion ver

apiVersion :: V.Version -> [Int]
apiVersion V.Version { V.versionBranch = ver } =
    case ver of
      (a : b : c : _) -> [a, b, c]
      (a : b : _)     -> [a, b, 0]
      (a : _)         -> [a, 0, 0]
      (_)             -> [0, 0, 0]

