{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Meta
  ( ropsMeta,
    -- * koshu-cop
    consKoshuCop, relkitKoshuCop,
    -- * koshu-cop-infix
    consKoshuCopInfix, relkitKoshuCopInfix,
    -- * koshu-source
    consKoshuSource, relmapKoshuSource,
    -- * koshu-rop
    consKoshuRop, relkitKoshuRop,
    -- * koshu-version
    consKoshuVersion, relkitKoshuVersion,
  ) where

import qualified Data.Version                  as V
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Message    as Msg


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
    --          CONSTRUCTOR        USAGE                 ATTRIBUTE
    [ Op.ropIJ  consKoshuAngleText "koshu-angle-text /N [/N]" "-name -text"
    , Op.ropV   consKoshuCop       "koshu-cop /N"        "-name"
    , Op.ropI   consKoshuCopInfix  "koshu-cop-infix /N [-height /N][-dir /N]"
                                                         "-name | -height -dir"
    , Op.ropI   consKoshuSource    "koshu-source /N [-name /N][-type /N]"
                                                         "-number | -name -type"
    , Op.ropV   consKoshuRop       "koshu-rop /N /N"     "-name | -group -usage"
    , Op.ropV   consKoshuVersion   "koshu-version /N"    "-term | -version"
    ]


-- ----------------------  koshu-cop

consKoshuCop :: C.CContent c => C.RopCons c
consKoshuCop use =
  do name <- Op.getTerm use "-name"
     Right $ relmapKoshuCop use name

relmapKoshuCop :: (C.CContent c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapKoshuCop use = C.relmapGlobal use . relkitKoshuCop

relkitKoshuCop :: (C.CContent c) => B.TermName -> C.RelkitGlobal c
relkitKoshuCop name g _ =
    Right $ C.relkitConstBody [name] $ map (B.li1 . C.pText . B.name) $ C.globalCops g


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
relkitKoshuCopInfix (name, height, dir) g _ = Right kit2 where
    kit2 = C.relkitJust he2 $ C.RelkitConst (map put $ C.globalInfix g)
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
relkitKoshuRop (name, group, usage) g _ = Right kit2 where
    kit2  = C.relkitConstBody ns bo2
    ns    = B.catMaybes [group, name, usage]
    bo2   = map f $ C.globalRops g
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
       _       -> Msg.unexpAttr ""
  where
    check n f t = do
      from <- C.literal undefined f
      to   <- C.literal undefined t
      Right $ C.relmapGlobal use $ relkitKoshuVersionCheck (from, to) n

relkitKoshuVersion :: (C.CContent c) => B.TermName -> C.Global c -> Maybe B.Head -> B.Ab (C.Relkit c)
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


-- ----------------------  koshu-source

--  koshu-source /number -type /type -name /name

consKoshuSource :: (C.CContent c) => C.RopCons c
consKoshuSource use =
  do num  <- Op.getTerm use "-number"
     ty   <- Op.getMaybe Op.getTerm use "-type"
     name <- Op.getMaybe Op.getTerm use "-name"
     Right $ relmapKoshuSource use (num, ty, name)

relmapKoshuSource :: (C.CContent c) => C.RopUse c -> (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuSource use = C.relmapGlobal use . relkitKoshuSource

relkitKoshuSource :: (C.CContent c) => (B.TermName, Maybe B.TermName, Maybe B.TermName) -> (C.RelkitGlobal c)
relkitKoshuSource (num, ty, name) C.Global { C.globalSources = res } _ = Right kit2 where
    ns         =  B.catMaybes [Just num, ty, name]
    kit2       =  C.relkitConstBody ns $ map assn res
    assn r     =  B.catMaybes [resNum r, resType r, resName r]

    resNum    =  Just         . C.pDecFromInt . B.sourceNumber
    resType   =  maybeAs ty   . C.pText       . B.sourceType
    resName   =  maybeAs name . C.pText       . B.sourceText

maybeAs :: Maybe a -> b -> Maybe b
maybeAs (Just _)  c  =  Just c
maybeAs (Nothing) _  =  Nothing


-- ----------------------  koshu-angle-text

--  koshu-angle-text /name
--  koshu-angle-text /name /text

consKoshuAngleText :: (Ord c, C.CText c) => C.RopCons c
consKoshuAngleText use =
  do n <- Op.getTerm use "-name"
     c <- Op.getMaybe Op.getTerm use "-text"
     Right $ relmapKoshuAngleText use (n, c)

relmapKoshuAngleText :: (Ord c, C.CText c) => C.RopUse c -> (B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuAngleText use = C.relmapFlow use . relkitKoshuAngleText

relkitKoshuAngleText :: (Ord c, C.CText c) => (B.TermName, Maybe B.TermName) -> Maybe B.Head -> B.Ab (C.Relkit c)
relkitKoshuAngleText (n, Just c) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n, c] assn
    assn (name, text) = [C.pText $ "<" ++ name ++ ">", C.pText $ text]
relkitKoshuAngleText (n, Nothing) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n] assn
    assn (name, _)    = [C.pText $ "<" ++ name ++ ">"]

koshuAngleTextBody :: (Ord c, C.CText c) => [B.TermName] -> ((String, String) -> [c]) -> C.Relkit c
koshuAngleTextBody he assn = C.relkitConstBody he $ B.sort $ map assn B.angleTexts
