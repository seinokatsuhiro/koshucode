{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for retrieving meta information.

module Koshucode.Baala.Rop.Flat.Meta
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

import qualified Data.Version                      as V
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Implementation of relational operators.
--
--   [@koshu-cop \/N@]
--     Retrieve list of content operators.
-- 
--   [@koshu-cop-infix \/N &#x5B; -height \/N &#x5D;&#x5B; -dir \/N &#x5D;@]
--     Retrieve list of infix specifications.
-- 
--   [@koshu-rop /N@]
--     Retrieve list of relmap operators.
-- 
--   [@koshu-version /N@]
--     Get version number of the koshu calculator.
-- 
ropsMeta :: (D.CContent c) => [C.Rop c]
ropsMeta = Rop.rops "meta"
    [ consKoshuAngleText O.& [ "koshu-angle-text /N [/N]"
                               O.& "-name -text?" ]
    , consKoshuCop       O.& [ "koshu-cop /N"
                               O.& "-name*" ]
    , consKoshuCopInfix  O.& [ "koshu-cop-infix /N [-height /N][-dir /N]"
                               O.& "-name . -height? -dir?" ]
    , consKoshuSource    O.& [ "koshu-source /N [-name /N][-type /N]"
                               O.& "-number . -name? -type?" ]
    , consKoshuRop       O.& [ "koshu-rop /N"
                               O.& "-name* . -group? -usage?" ]
    , consKoshuProxy     O.& [ "koshu-proxy /N /N"
                               O.& "-proto -uri" ]
    , consKoshuVersion   O.& [ "koshu-version /N"
                               O.& "-term* . -version?" ]
    ]


-- ----------------------  koshu-cop

-- | __koshu-cop \/N__
consKoshuCop :: D.CContent c => C.RopCons c
consKoshuCop med =
  do name <- Rop.getTerm med "-name"
     Right $ relmapKoshuCop med name

-- | Create @koshu-cop@ relmap.
relmapKoshuCop :: (D.CContent c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapKoshuCop med = C.relmapHook med . relkitKoshuCop

-- | Create @koshu-cop@ relkit.
relkitKoshuCop :: (D.CContent c) => S.TermName -> C.RelkitHook c
relkitKoshuCop name res _ =
    Right $ C.relkitConstBody [name] $ map (B.list1 . D.pText . B.name) $ C.globalCops g
          where g = C.getGlobal res


-- ----------------------  koshu-cop-infix

-- | __koshu-cop-infix \/N [ -height \/N ][ -dir \/N ]__
consKoshuCopInfix :: (D.CContent c) => C.RopCons c
consKoshuCopInfix med =
  do name   <- Rop.getTerm med "-name"
     height <- Rop.getMaybe Rop.getTerm med "-height"
     dir    <- Rop.getMaybe Rop.getTerm med "-dir"
     Right $ relmapKoshuCopInfix med (name, height, dir)

-- | Create @koshu-cop-infix@ relmap.
relmapKoshuCopInfix :: (D.CContent c) => C.Intmed c -> (S.TermName, Maybe S.TermName, Maybe S.TermName) -> C.Relmap c
relmapKoshuCopInfix med = C.relmapHook med . relkitKoshuCopInfix

-- | Create @koshu-cop-infix@ relkit.
relkitKoshuCopInfix :: (D.CContent c) => (S.TermName, Maybe S.TermName, Maybe S.TermName) -> C.RelkitHook c
relkitKoshuCopInfix (name, height, dir) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitJust he2 $ C.RelkitConst (map put $ C.globalInfix g)
    he2   = D.headFrom $ [name] ++ heightMaybe B.list1        ++ dirMaybe B.list1
    put (n, ih)  = [D.pText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = maybeEmpty height
    dirMaybe    = maybeEmpty dir

    heightTerm (Left  h) _ = [D.pInt h]
    heightTerm (Right h) _ = [D.pInt h]

    dirTerm    (Left  _) _ = [D.pText "left"]
    dirTerm    (Right _) _ = [D.pText "right"]

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

-- ----------------------  koshu-rop

-- | __koshu-rop \/N [ -group \/N ][ -usage \/N ]__
consKoshuRop :: (D.CContent c) => C.RopCons c
consKoshuRop med =
  do name  <- Rop.getTerm med "-name"
     group <- Rop.getMaybe Rop.getTerm med "-group"
     usage <- Rop.getMaybe Rop.getTerm med "-usage"
     Right $ relmapKoshuRop med (Just name, group, usage)

-- | Create @koshu-rop@ relmap.
relmapKoshuRop :: (D.CContent c)
    => C.Intmed c -> (Maybe S.TermName, Maybe S.TermName, Maybe S.TermName)
    -> C.Relmap c
relmapKoshuRop med = C.relmapHook med . relkitKoshuRop

-- | Create @koshu-rop@ relkit.
relkitKoshuRop :: (D.CContent c)
    => (Maybe S.TermName, Maybe S.TermName, Maybe S.TermName)
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
          cond (Just _) get = [D.pText $ get rop]
          cond _ _ = []


-- ----------------------  koshu-proxy

-- | __koshu-proxy \/N \/N__
consKoshuProxy :: (D.CContent c) => C.RopCons c
consKoshuProxy med =
  do proto  <- Rop.getTerm med "-proto"
     uri    <- Rop.getTerm med "-uri"
     Right $ relmapKoshuProxy med (Just proto, Just uri)

-- | Create @koshu-proxy@ relmap.
relmapKoshuProxy :: (D.CContent c)
    => C.Intmed c -> (Maybe S.TermName, Maybe S.TermName)
    -> C.Relmap c
relmapKoshuProxy med = C.relmapHook med . relkitKoshuProxy

-- | Create @koshu-proxy@ relkit.
relkitKoshuProxy :: (D.CContent c)
    => (Maybe S.TermName, Maybe S.TermName)
    -> C.RelkitHook c
relkitKoshuProxy (proto, uri) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitConstBody ns bo2
    ns    = B.catMaybes [proto, uri]
    bo2   = map f $ C.globalProxy g
    f proxy = the proto (D.pText . fst) ++
              the uri   (D.maybeEmpty D.pText . snd)
        where
          the (Just _) get  = [get proxy]
          the _ _           = []


-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

-- | __koshu-version \/N__
consKoshuVersion :: (D.CContent c) => C.RopCons c
consKoshuVersion med =
  do n   <- Rop.getTerm  med "-term"
     ver <- Rop.getMaybe Rop.getTrees med "-version"
     case ver of
       Nothing      -> Right $ C.relmapHook med $ relkitKoshuVersion n
       Just [f]     -> check n f f
       Just [f, t]  -> check n f t
       _            -> Msg.unexpAttr ""
  where
    check n f t = do
      from <- D.treeContent f
      to   <- D.treeContent t
      Right $ C.relmapHook med $ relkitKoshuVersionCheck (from, to) n

-- | Create @koshu-version@ relkit.
relkitKoshuVersion :: (D.CContent c) => S.TermName -> C.RelkitHook c
relkitKoshuVersion n h _ =
    Right $ C.relkitConstSingleton [n] [ D.pList $ map D.pInt $ apiVersion ver ] where
        ver = C.globalVersion $ C.getGlobal h

relkitKoshuVersionCheck :: (D.CContent c) => (c, c) -> S.TermName -> C.RelkitHook c
relkitKoshuVersionCheck (from, to) n h _
    | verC >= from && verC <= to  = Right kitV
    | otherwise                   = Right kitE
    where ver  = C.globalVersion $ C.getGlobal h
          verC = D.pList $ map D.pInt $ apiVersion ver
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

-- | __koshu-source \/N [ -type \/N ][ -name \/N ]__
consKoshuSource :: (D.CContent c) => C.RopCons c
consKoshuSource med =
  do num  <- Rop.getTerm med "-number"
     ty   <- Rop.getMaybe Rop.getTerm med "-type"
     name <- Rop.getMaybe Rop.getTerm med "-name"
     Right $ relmapKoshuSource med (num, ty, name)

-- | Create @koshu-source@ relmap.
relmapKoshuSource :: (D.CContent c) => C.Intmed c -> (S.TermName, Maybe S.TermName, Maybe S.TermName) -> C.Relmap c
relmapKoshuSource med = C.relmapHook med . relkitKoshuSource

-- | Create @koshu-source@ relkit.
relkitKoshuSource :: (D.CContent c) => (S.TermName, Maybe S.TermName, Maybe S.TermName) -> C.RelkitHook c
relkitKoshuSource (num, ty, name) h _ = Right kit2 where
    code       = C.resIncluded h
    ns         = B.catMaybes [Just num, ty, name]
    kit2       = C.relkitConstBody ns $ map assn code
    assn c     = B.catMaybes [codeNo c, codeType c, codeText c]

    codeNo     = Just         . D.pInt  . O.getIx
    codeType   = maybeAs ty   . D.pText . B.ioPointType . B.nioPoint
    codeText   = maybeAs name . D.pText . B.ioPointText . B.nioPoint

maybeAs :: Maybe a -> b -> Maybe b
maybeAs (Just _)  c  =  Just c
maybeAs (Nothing) _  =  Nothing


-- ----------------------  koshu-angle-text

--  koshu-angle-text /name
--  koshu-angle-text /name /text

consKoshuAngleText :: (Ord c, D.CText c) => C.RopCons c
consKoshuAngleText med =
  do n <- Rop.getTerm med "-name"
     c <- Rop.getMaybe Rop.getTerm med "-text"
     Right $ relmapKoshuAngleText med (n, c)

-- | Create @koshu-angle-text@ relmap.
relmapKoshuAngleText :: (Ord c, D.CText c) => C.Intmed c -> (S.TermName, Maybe S.TermName) -> C.Relmap c
relmapKoshuAngleText med = C.relmapFlow med . relkitKoshuAngleText

-- | Create @koshu-angle-text@ relkit.
relkitKoshuAngleText :: (Ord c, D.CText c) => (S.TermName, Maybe S.TermName) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitKoshuAngleText (n, Just c) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n, c] assn
    assn (name, text) = [D.pText $ "<" ++ name ++ ">", D.pText $ text]
relkitKoshuAngleText (n, Nothing) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n] assn
    assn (name, _)    = [D.pText $ "<" ++ name ++ ">"]

koshuAngleTextBody :: (Ord c, D.CText c) => [S.TermName] -> ((String, String) -> [c]) -> C.Relkit c
koshuAngleTextBody he assn = C.relkitConstBody he $ B.sort $ map assn S.angleTexts
