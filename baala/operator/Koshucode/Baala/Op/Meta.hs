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
    -- * koshu-proxy
    consKoshuProxy, relkitKoshuProxy,
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
    --        CONSTRUCTOR        USAGE                       ATTRIBUTE
    [ Op.def  consKoshuAngleText "koshu-angle-text /N [/N]" "1? -name -text"
    , Op.def  consKoshuCop       "koshu-cop /N"              "V -name"
    , Op.def  consKoshuCopInfix  "koshu-cop-infix /N [-height /N][-dir /N]"
                                                             "1 -name | -height -dir"
    , Op.def  consKoshuSource    "koshu-source /N [-name /N][-type /N]"
                                                             "1 -number | -name -type"
    , Op.def  consKoshuRop       "koshu-rop /N /N"           "V -name | -group -usage"
    , Op.def  consKoshuProxy     "koshu-proxy /N /N"         "2 -proto -uri"
    , Op.def  consKoshuVersion   "koshu-version /N"          "V -term | -version"
    ]


-- ----------------------  koshu-cop

consKoshuCop :: C.CContent c => C.RopCons c
consKoshuCop med =
  do name <- Op.getTerm med "-name"
     Right $ relmapKoshuCop med name

relmapKoshuCop :: (C.CContent c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapKoshuCop med = C.relmapHook med . relkitKoshuCop

relkitKoshuCop :: (C.CContent c) => B.TermName -> C.RelkitHook c
relkitKoshuCop name res _ =
    Right $ C.relkitConstBody [name] $ map (B.li1 . C.pText . B.name) $ C.globalCops g
          where g = C.getGlobal res


-- ----------------------  koshu-cop-infix

consKoshuCopInfix :: (C.CContent c) => C.RopCons c
consKoshuCopInfix med =
  do name   <- Op.getTerm med "-name"
     height <- Op.getMaybe Op.getTerm med "-height"
     dir    <- Op.getMaybe Op.getTerm med "-dir"
     Right $ relmapKoshuCopInfix med (name, height, dir)

relmapKoshuCopInfix :: (C.CContent c) => C.Intmed c -> (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuCopInfix med = C.relmapHook med . relkitKoshuCopInfix

relkitKoshuCopInfix :: (C.CContent c) => (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.RelkitHook c
relkitKoshuCopInfix (name, height, dir) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitJust he2 $ C.RelkitConst (map put $ C.globalInfix g)
    he2   = B.headFrom $ [name] ++ heightMaybe B.li1           ++ dirMaybe B.li1
    put (n, ih)  = [C.pText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = B.maybeEmpty height
    dirMaybe    = B.maybeEmpty dir

    heightTerm (Left  h) _ = [C.pDecFromInt h]
    heightTerm (Right h) _ = [C.pDecFromInt h]

    dirTerm    (Left  _) _ = [C.pText "left"]
    dirTerm    (Right _) _ = [C.pText "right"]


-- ----------------------  koshu-rop

consKoshuRop :: (C.CContent c) => C.RopCons c
consKoshuRop med =
  do name  <- Op.getTerm med "-name"
     group <- Op.getMaybe Op.getTerm med "-group"
     usage <- Op.getMaybe Op.getTerm med "-usage"
     Right $ relmapKoshuRop med (Just name, group, usage)

relmapKoshuRop :: (C.CContent c)
    => C.Intmed c -> (Maybe B.TermName, Maybe B.TermName, Maybe B.TermName)
    -> C.Relmap c
relmapKoshuRop med = C.relmapHook med . relkitKoshuRop

relkitKoshuRop :: (C.CContent c)
    => (Maybe B.TermName, Maybe B.TermName, Maybe B.TermName)
    -> C.RelkitHook c
relkitKoshuRop (name, group, usage) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitConstBody ns bo2
    ns    = B.catMaybes [group, name, usage]
    bo2   = map f $ C.globalRops g
    f rop = cond group C.ropGroup ++
            cond name  C.ropName  ++
            cond usage C.ropUsage
        where
          cond (Just _) get = [C.pText $ get rop]
          cond _ _ = []


-- ----------------------  koshu-proxy

consKoshuProxy :: (C.CContent c) => C.RopCons c
consKoshuProxy med =
  do proto  <- Op.getTerm med "-proto"
     uri    <- Op.getTerm med "-uri"
     Right $ relmapKoshuProxy med (Just proto, Just uri)

relmapKoshuProxy :: (C.CContent c)
    => C.Intmed c -> (Maybe B.TermName, Maybe B.TermName)
    -> C.Relmap c
relmapKoshuProxy med = C.relmapHook med . relkitKoshuProxy

relkitKoshuProxy :: (C.CContent c)
    => (Maybe B.TermName, Maybe B.TermName)
    -> C.RelkitHook c
relkitKoshuProxy (proto, uri) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitConstBody ns bo2
    ns    = B.catMaybes [proto, uri]
    bo2   = map f $ C.globalProxy g
    f proxy = the proto (C.pText . fst) ++
              the uri   (C.maybeEmpty C.pText . snd)
        where
          the (Just _) get  = [get proxy]
          the _ _           = []


-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

consKoshuVersion :: (C.CContent c) => C.RopCons c
consKoshuVersion med =
  do n   <- Op.getTerm  med "-term"
     ver <- Op.getTrees med "-version"
     case ver of
       []      -> Right $ C.relmapHook med $ relkitKoshuVersion n
       [f]     -> check n f f
       [f, t]  -> check n f t
       _       -> Msg.unexpAttr ""
  where
    check n f t = do
      from <- C.contentCons undefined f
      to   <- C.contentCons undefined t
      Right $ C.relmapHook med $ relkitKoshuVersionCheck (from, to) n

relkitKoshuVersion :: (C.CContent c) => B.TermName -> C.RelkitHook c
relkitKoshuVersion n h _ =
    Right $ C.relkitConstSingleton [n] [ C.pList $ map C.pDecFromInt $ apiVersion ver ] where
        ver = C.globalVersion $ C.getGlobal h

relkitKoshuVersionCheck :: (C.CContent c) => (c, c) -> B.TermName -> C.RelkitHook c
relkitKoshuVersionCheck (from, to) n h _
    | verC >= from && verC <= to  = Right kitV
    | otherwise                   = Right kitE
    where ver  = C.globalVersion $ C.getGlobal h
          verC = C.pList $ map C.pDecFromInt $ apiVersion ver
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
consKoshuSource med =
  do num  <- Op.getTerm med "-number"
     ty   <- Op.getMaybe Op.getTerm med "-type"
     name <- Op.getMaybe Op.getTerm med "-name"
     Right $ relmapKoshuSource med (num, ty, name)

relmapKoshuSource :: (C.CContent c) => C.Intmed c -> (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuSource med = C.relmapHook med . relkitKoshuSource

relkitKoshuSource :: (C.CContent c) => (B.TermName, Maybe B.TermName, Maybe B.TermName) -> C.RelkitHook c
relkitKoshuSource (num, ty, name) h _ = Right kit2 where
    code       = C.resIncluded h
    ns         = B.catMaybes [Just num, ty, name]
    kit2       = C.relkitConstBody ns $ map assn code
    assn c     = B.catMaybes [codeNo c, codeType c, codeText c]

    codeNo     = Just         . C.pDecFromInt . B.codeNumber
    codeType   = maybeAs ty   . C.pText       . B.ioPointType . B.codeName
    codeText   = maybeAs name . C.pText       . B.ioPointText . B.codeName

maybeAs :: Maybe a -> b -> Maybe b
maybeAs (Just _)  c  =  Just c
maybeAs (Nothing) _  =  Nothing


-- ----------------------  koshu-angle-text

--  koshu-angle-text /name
--  koshu-angle-text /name /text

consKoshuAngleText :: (Ord c, C.CText c) => C.RopCons c
consKoshuAngleText med =
  do n <- Op.getTerm med "-name"
     c <- Op.getMaybe Op.getTerm med "-text"
     Right $ relmapKoshuAngleText med (n, c)

relmapKoshuAngleText :: (Ord c, C.CText c) => C.Intmed c -> (B.TermName, Maybe B.TermName) -> C.Relmap c
relmapKoshuAngleText med = C.relmapFlow med . relkitKoshuAngleText

relkitKoshuAngleText :: (Ord c, C.CText c) => (B.TermName, Maybe B.TermName) -> Maybe B.Head -> B.Ab (C.Relkit c)
relkitKoshuAngleText (n, Just c) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n, c] assn
    assn (name, text) = [C.pText $ "<" ++ name ++ ">", C.pText $ text]
relkitKoshuAngleText (n, Nothing) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n] assn
    assn (name, _)    = [C.pText $ "<" ++ name ++ ">"]

koshuAngleTextBody :: (Ord c, C.CText c) => [B.TermName] -> ((String, String) -> [c]) -> C.Relkit c
koshuAngleTextBody he assn = C.relkitConstBody he $ B.sort $ map assn B.angleTexts
