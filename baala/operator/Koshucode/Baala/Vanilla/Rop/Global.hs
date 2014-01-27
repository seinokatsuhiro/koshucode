{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Global
( 
  -- * koshu-cop
  ropConsKoshuCop,
  -- * koshu-cop-infix
  ropConsKoshuCopInfix,
  -- * koshu-rop
  ropConsKoshuRop,
  -- * koshu-version
  ropConsKoshuVersion,
) where

import qualified Data.Version                  as V
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop



-- ----------------------  koshu-cop

ropConsKoshuCop :: C.CContent c => C.RopCons c
ropConsKoshuCop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuCop use name = C.relmapGlobal use $ relfyKoshuCop name

relfyKoshuCop :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuCop name C.Global { C.globalCops = (cops, _) } _ =
    Right $C.relfyConstBody [name] $ map (B.singleton . C.putText . B.name) cops



-- ----------------------  koshu-cop-infix

ropConsKoshuCopInfix :: (C.CContent c) => C.RopCons c
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

ropConsKoshuRop :: (C.CContent c) => C.RopCons c
ropConsKoshuRop use =
  do name <- Rop.getTerm use "-name"
     Right $ relmapKoshuRop use name

relmapKoshuRop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuRop use name = C.relmapGlobal use $ relfyKoshuRop name

relfyKoshuRop :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuRop name C.Global { C.globalRops = rops } _ =
    Right $C.relfyConstBody [name] $ map (B.singleton . C.putText . C.ropName) rops

-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

ropConsKoshuVersion :: (C.CContent c) => C.RopCons c
ropConsKoshuVersion use =
  do n   <- Rop.getTerm  use "-term"
     ver <- Rop.getTrees use "-version"
     case ver of
       []      -> Right $ C.relmapGlobal use $ relfyKoshuVersion n
       [f]     -> check n f f
       [f, t]  -> check n f t
       _       -> Left $ B.AbortAnalysis [] $ B.AAMalformedOperand ""
  where
    check n f t = do
      from <- C.litContent f
      to   <- C.litContent t
      Right $ C.relmapGlobal use $ relfyKoshuVersionCheck (from, to) n

relfyKoshuVersion :: (C.CContent c) => B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuVersion n C.Global { C.globalVersion = ver } _ =
    Right $ C.relfyConstSingleton [n] [ C.putList $ map C.putDecFromInt $ apiVersion ver ]

relfyKoshuVersionCheck :: (C.CContent c) => (c, c) -> B.Termname -> C.Global c -> B.Relhead -> B.Ab (C.Relfy c)
relfyKoshuVersionCheck (from, to) n C.Global { C.globalVersion = ver } _
    | version >= from && version <= to  = Right $ C.relfyConstSingleton [n] [version] -- todo
    | otherwise = Right $ C.relfyConstEmpty [n]
    where version = C.putList $ map C.putDecFromInt $ apiVersion ver

apiVersion :: V.Version -> [Int]
apiVersion V.Version { V.versionBranch = ver } =
    case ver of
      (a : b : c : _) -> [a, b, c]
      (a : b : _)     -> [a, b, 0]
      (a : _)         -> [a, 0, 0]
      (_)             -> [0, 0, 0]
