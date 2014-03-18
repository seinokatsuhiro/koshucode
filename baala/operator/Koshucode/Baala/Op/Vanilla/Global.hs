{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Global
( 
  -- * do
  consDo,
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
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Message    as Message


-- ----------------------  do

consDo :: (Ord c) => C.RopCons c
consDo use =
  do rmap <- Op.getRelmap use
     treesLet <- Op.getWordTrees use "-let"
     let C.RelmapCons lx full = C.relmapCons $ C.ropGlobal use
         (names, trees) = unzip treesLet
     lxs   <- mapM lx $ map B.singleton trees
     fulls <- mapM full lxs
     Right $ undefined


-- ----------------------  koshu-cop

consKoshuCop :: C.CContent c => C.RopCons c
consKoshuCop use =
  do name <- Op.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuCop use = C.relmapGlobal use . relkitKoshuCop

relkitKoshuCop :: (C.CContent c) => B.Termname -> C.RelkitGlobal c
relkitKoshuCop name C.Global { C.globalCops = (cops, _) } _ =
    Right $ C.relkitConstBody [name] $ map (B.singleton . C.pText . B.name) cops


-- ----------------------  koshu-cop-infix

consKoshuCopInfix :: (C.CContent c) => C.RopCons c
consKoshuCopInfix use =
  do name   <- Op.getTerm use "-name"
     height <- Op.getMaybe Op.getTerm use "-height"
     dir    <- Op.getMaybe Op.getTerm use "-dir"
     Right $ relmapKoshuCopInfix use (name, height, dir)

relmapKoshuCopInfix :: (C.CContent c) => C.RopUse c -> (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.Relmap c
relmapKoshuCopInfix use = C.relmapGlobal use . relkitKoshuCopInfix

relkitKoshuCopInfix :: (C.CContent c) => (B.Termname, Maybe B.Termname, Maybe B.Termname) -> C.RelkitGlobal c
relkitKoshuCopInfix (name, height, dir) C.Global { C.globalCops = (_, htab) } _ = Right kit2 where
    kit2 = C.relkitJust he2 $ C.RelkitConst (map put htab)
    he2  = B.headFrom $ [name] ++ heightMaybe B.singleton     ++ dirMaybe B.singleton
    put (n, ih)  = [C.pText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = B.maybeEmpty height
    dirMaybe    = B.maybeEmpty dir

    heightTerm (Left  h) _ = [C.pDecFromInt h]
    heightTerm (Right h) _ = [C.pDecFromInt h]

    dirTerm    (Left  _) _ = [C.pText "left"]
    dirTerm    (Right _) _ = [C.pText "right"]


-- ----------------------  koshu-rop

consKoshuRop :: (C.CContent c) => C.RopCons c
consKoshuRop use =
  do name <- Op.getTerm use "-name"
     Right $ relmapKoshuRop use name

relmapKoshuRop :: (C.CContent c) => C.RopUse c -> B.Termname -> C.Relmap c
relmapKoshuRop use = C.relmapGlobal use . relkitKoshuRop

relkitKoshuRop :: (C.CContent c) => B.Termname -> C.RelkitGlobal c
relkitKoshuRop name C.Global { C.globalRops = rops } _ = Right kit2 where
    kit2 = C.relkitConstBody [name] bo2
    bo2  = map (B.singleton . C.pText . C.ropName) rops


-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

consKoshuVersion :: (C.CContent c) => C.RopCons c
consKoshuVersion use =
  do n   <- Op.getTerm  use "-term"
     ver <- Op.getTrees use "-version"
     case ver of
       []      -> Right $ C.relmapGlobal use $ relkitKoshuVersion n
       [f]     -> check n f f
       [f, t]  -> check n f t
       _       -> Message.unexpOperand ""
  where
    check n f t = do
      from <- C.litContent f
      to   <- C.litContent t
      Right $ C.relmapGlobal use $ relkitKoshuVersionCheck (from, to) n

relkitKoshuVersion :: (C.CContent c) => B.Termname -> C.Global c -> Maybe B.Relhead -> B.Ab (C.Relkit c)
relkitKoshuVersion n C.Global { C.globalVersion = ver } _ =
    Right $ C.relkitConstSingleton [n] [ C.pList $ map C.pDecFromInt $ apiVersion ver ]

relkitKoshuVersionCheck :: (C.CContent c) => (c, c) -> B.Termname -> C.RelkitGlobal c
relkitKoshuVersionCheck (from, to) n C.Global { C.globalVersion = ver } _
    | verC >= from && verC <= to  = Right kitV
    | otherwise                   = Right kitE
    where verC = C.pList $ map C.pDecFromInt $ apiVersion ver
          kitV = C.relkitConstSingleton [n] [verC] -- todo
          kitE = C.relkitConstEmpty [n]

apiVersion :: V.Version -> [Int]
apiVersion V.Version { V.versionBranch = ver } =
    case ver of
      (a : b : c : _) -> [a, b, c]
      (a : b : _)     -> [a, b, 0]
      (a : _)         -> [a, 0, 0]
      (_)             -> [0, 0, 0]

