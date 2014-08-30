{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Meta
( ropsMeta,
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


-- | Implementation of relational operators.
--
--   [@koshu-cop \/N@]
--     Retrieve list of content operators.
-- 
--   [@koshu-cop-infix \/N \[ -height \/N \]\[ -dir \/N \]@]
--     Retrieve list of infix specifications.
-- 
--   [@koshu-rop /N@]
--     Retrieve list of relmap operators.
-- 
--   [@koshu-version /N@]
--     Get version number of the koshu calculator.
-- 
ropsMeta :: (C.CContent c) => [C.Rop c]
ropsMeta = Op.ropList "meta"
    --         CONSTRUCTOR       USAGE                ATTRIBUTE
    [ Op.ropV  consKoshuCop      "koshu-cop /N"       "-name"
    , Op.ropI  consKoshuCopInfix "koshu-cop-infix /N [-height /N][-dir /N]"
                                                      "-name | -height -dir"
    , Op.ropV  consKoshuRop      "koshu-rop /N /N"    "-name | -group -usage"
    , Op.ropV  consKoshuVersion  "koshu-version /N"   "-term | -version"
    ]


-- ----------------------  koshu-cop

consKoshuCop :: C.CContent c => C.RopCons c
consKoshuCop use =
  do name <- Op.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapKoshuCop use = C.relmapGlobal use . relkitKoshuCop

relkitKoshuCop :: (C.CContent c) => B.TermName -> C.RelkitGlobal c
relkitKoshuCop name C.Global { C.globalCops = (cops, _) } _ =
    Right $ C.relkitConstBody [name] $ map (B.li1 . C.pText . B.name) cops


-- ----------------------  koshu-cop-infix

consKoshuCopInfix :: (C.CContent c) => C.RopCons c
consKoshuCopInfix use =
  do name   <- Op.getTerm use "-name"
     height <- Op.getMaybe Op.getTerm use "-height"
     dir    <- Op.getMaybe Op.getTerm use "-dir"
     Right $ relmapKoshuCopInfix use (name, height, dir)

relmapKoshuCopInfix :: (C.CContent c) => C.RopUse c -> (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuCopInfix use = C.relmapGlobal use . relkitKoshuCopInfix

relkitKoshuCopInfix :: (C.CContent c) => (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.RelkitGlobal c
relkitKoshuCopInfix (name, height, dir) C.Global { C.globalCops = (_, htab) } _ = Right kit2 where
    kit2 = C.relkitJust he2 $ C.RelkitConst (map put htab)
    he2  = B.headFrom $ [name] ++ heightMaybe B.li1           ++ dirMaybe B.li1
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
  do name  <- Op.getTerm use "-name"
     group <- Op.getMaybe Op.getTerm use "-group"
     usage <- Op.getMaybe Op.getTerm use "-usage"
     Right $ relmapKoshuRop use (Just name, group, usage)

relmapKoshuRop :: (C.CContent c)
    => C.RopUse c -> (Maybe B.TermName, Maybe B.TermName, Maybe B.TermName)
    -> C.Relmap c
relmapKoshuRop use = C.relmapGlobal use . relkitKoshuRop

relkitKoshuRop :: (C.CContent c)
    => (Maybe B.TermName, Maybe B.TermName, Maybe B.TermName)
    -> C.RelkitGlobal c
relkitKoshuRop (name, group, usage) C.Global { C.globalRops = rops } _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = B.catMaybes [group, name, usage]
    bo2   = map f rops
    f rop = cond group C.ropGroup ++
            cond name  C.ropName  ++
            cond usage C.ropUsage
        where
          cond (Just _) get = [C.pText $ get rop]
          cond _ _ = []



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
       _       -> Message.unexpAttr ""
  where
    check n f t = do
      from <- C.literal undefined f
      to   <- C.literal undefined t
      Right $ C.relmapGlobal use $ relkitKoshuVersionCheck (from, to) n

relkitKoshuVersion :: (C.CContent c) => B.TermName -> C.Global c -> Maybe B.Relhead -> B.Ab (C.Relkit c)
relkitKoshuVersion n C.Global { C.globalVersion = ver } _ =
    Right $ C.relkitConstSingleton [n] [ C.pList $ map C.pDecFromInt $ apiVersion ver ]

relkitKoshuVersionCheck :: (C.CContent c) => (c, c) -> B.TermName -> C.RelkitGlobal c
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

