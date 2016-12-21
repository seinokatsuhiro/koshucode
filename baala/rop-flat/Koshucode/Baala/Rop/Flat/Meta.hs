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
import qualified Koshucode.Baala.DataPlus          as K
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
ropsMeta :: (K.CContent c) => [C.Rop c]
ropsMeta = Rop.rops "meta"
    [ consKoshuAngleText K.& [ "koshu-angle-text /N [/N]"
                               K.& "-name -text?" ]
    , consKoshuCop       K.& [ "koshu-cop /N"
                               K.& "-name*" ]
    , consKoshuCopInfix  K.& [ "koshu-cop-infix /N [-height /N][-dir /N]"
                               K.& "-name . -height? -dir?" ]
    , consKoshuSource    K.& [ "koshu-source /N [-name /N][-type /N]"
                               K.& "-number . -name? -type?" ]
    , consKoshuRop       K.& [ "koshu-rop /N"
                               K.& "-name* . -group? -usage?" ]
    , consKoshuProxy     K.& [ "koshu-proxy /N /N"
                               K.& "-proto -uri" ]
    , consKoshuVersion   K.& [ "koshu-version /N"
                               K.& "-term* . -version?" ]
    ]


-- ----------------------  koshu-cop

-- | __koshu-cop \/N__
consKoshuCop :: K.CContent c => C.RopCons c
consKoshuCop med =
  do name <- Rop.getTerm med "-name"
     Right $ relmapKoshuCop med name

-- | Create @koshu-cop@ relmap.
relmapKoshuCop :: (K.CContent c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapKoshuCop med = C.relmapHook med . relkitKoshuCop

-- | Create @koshu-cop@ relkit.
relkitKoshuCop :: (K.CContent c) => K.TermName -> C.RelkitHook c
relkitKoshuCop name res _ =
    Right $ C.relkitConstBody [name] $ map (K.list1 . K.pText . K.name) $ C.globalCops g
          where g = C.getGlobal res


-- ----------------------  koshu-cop-infix

-- | __koshu-cop-infix \/N [ -height \/N ][ -dir \/N ]__
consKoshuCopInfix :: (K.CContent c) => C.RopCons c
consKoshuCopInfix med =
  do name   <- Rop.getTerm med "-name"
     height <- Rop.getMaybe Rop.getTerm med "-height"
     dir    <- Rop.getMaybe Rop.getTerm med "-dir"
     Right $ relmapKoshuCopInfix med (name, height, dir)

-- | Create @koshu-cop-infix@ relmap.
relmapKoshuCopInfix :: (K.CContent c) => C.Intmed c -> (K.TermName, Maybe K.TermName, Maybe K.TermName) -> C.Relmap c
relmapKoshuCopInfix med = C.relmapHook med . relkitKoshuCopInfix

-- | Create @koshu-cop-infix@ relkit.
relkitKoshuCopInfix :: (K.CContent c) => (K.TermName, Maybe K.TermName, Maybe K.TermName) -> C.RelkitHook c
relkitKoshuCopInfix (name, height, dir) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitJust he2 $ C.RelkitConst (map put $ C.globalInfix g)
    he2   = K.headFrom $ [name] ++ heightMaybe K.list1        ++ dirMaybe K.list1
    put (n, ih)  = [K.pText n] ++ heightMaybe (heightTerm ih) ++ dirMaybe (dirTerm ih)

    heightMaybe = maybeEmpty height
    dirMaybe    = maybeEmpty dir

    heightTerm (Left  h) _ = [K.pInt h]
    heightTerm (Right h) _ = [K.pInt h]

    dirTerm    (Left  _) _ = [K.pText "left"]
    dirTerm    (Right _) _ = [K.pText "right"]

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

-- ----------------------  koshu-rop

-- | __koshu-rop \/N [ -group \/N ][ -usage \/N ]__
consKoshuRop :: (K.CContent c) => C.RopCons c
consKoshuRop med =
  do name  <- Rop.getTerm med "-name"
     group <- Rop.getMaybe Rop.getTerm med "-group"
     usage <- Rop.getMaybe Rop.getTerm med "-usage"
     Right $ relmapKoshuRop med (Just name, group, usage)

-- | Create @koshu-rop@ relmap.
relmapKoshuRop :: (K.CContent c)
    => C.Intmed c -> (Maybe K.TermName, Maybe K.TermName, Maybe K.TermName)
    -> C.Relmap c
relmapKoshuRop med = C.relmapHook med . relkitKoshuRop

-- | Create @koshu-rop@ relkit.
relkitKoshuRop :: (K.CContent c)
    => (Maybe K.TermName, Maybe K.TermName, Maybe K.TermName)
    -> C.RelkitHook c
relkitKoshuRop (name, group, usage) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitConstBody ns bo2
    ns    = K.catMaybes [group, name, usage]
    bo2   = map f $ C.globalRops g
    f rop = cond group C.ropGroup ++
            cond name  C.ropName  ++
            cond usage C.ropUsage
        where
          cond (Just _) get = [K.pText $ get rop]
          cond _ _ = []


-- ----------------------  koshu-proxy

-- | __koshu-proxy \/N \/N__
consKoshuProxy :: (K.CContent c) => C.RopCons c
consKoshuProxy med =
  do proto  <- Rop.getTerm med "-proto"
     uri    <- Rop.getTerm med "-uri"
     Right $ relmapKoshuProxy med (Just proto, Just uri)

-- | Create @koshu-proxy@ relmap.
relmapKoshuProxy :: (K.CContent c)
    => C.Intmed c -> (Maybe K.TermName, Maybe K.TermName)
    -> C.Relmap c
relmapKoshuProxy med = C.relmapHook med . relkitKoshuProxy

-- | Create @koshu-proxy@ relkit.
relkitKoshuProxy :: (K.CContent c)
    => (Maybe K.TermName, Maybe K.TermName)
    -> C.RelkitHook c
relkitKoshuProxy (proto, uri) res _ = Right kit2 where
    g     = C.getGlobal res
    kit2  = C.relkitConstBody ns bo2
    ns    = K.catMaybes [proto, uri]
    bo2   = map f $ C.globalProxy g
    f proxy = the proto (K.pText . fst) ++
              the uri   (K.maybeEmpty K.pText . snd)
        where
          the (Just _) get  = [get proxy]
          the _ _           = []


-- ----------------------  koshu-version

--  koshu-version /ver
--  koshu-version /ver [1:0]
--  koshu-version /ver [1:0] [1:2]

-- | __koshu-version \/N__
consKoshuVersion :: (K.CContent c) => C.RopCons c
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
      from <- K.treeContent f
      to   <- K.treeContent t
      Right $ C.relmapHook med $ relkitKoshuVersionCheck (from, to) n

-- | Create @koshu-version@ relkit.
relkitKoshuVersion :: (K.CContent c) => K.TermName -> C.RelkitHook c
relkitKoshuVersion n h _ =
    Right $ C.relkitConstSingleton [n] [ K.pList $ map K.pInt $ apiVersion ver ] where
        ver = C.globalVersion $ C.getGlobal h

relkitKoshuVersionCheck :: (K.CContent c) => (c, c) -> K.TermName -> C.RelkitHook c
relkitKoshuVersionCheck (from, to) n h _
    | verC >= from && verC <= to  = Right kitV
    | otherwise                   = Right kitE
    where ver  = C.globalVersion $ C.getGlobal h
          verC = K.pList $ map K.pInt $ apiVersion ver
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
consKoshuSource :: (K.CContent c) => C.RopCons c
consKoshuSource med =
  do num  <- Rop.getTerm med "-number"
     ty   <- Rop.getMaybe Rop.getTerm med "-type"
     name <- Rop.getMaybe Rop.getTerm med "-name"
     Right $ relmapKoshuSource med (num, ty, name)

-- | Create @koshu-source@ relmap.
relmapKoshuSource :: (K.CContent c) => C.Intmed c -> (K.TermName, Maybe K.TermName, Maybe K.TermName) -> C.Relmap c
relmapKoshuSource med = C.relmapHook med . relkitKoshuSource

-- | Create @koshu-source@ relkit.
relkitKoshuSource :: (K.CContent c) => (K.TermName, Maybe K.TermName, Maybe K.TermName) -> C.RelkitHook c
relkitKoshuSource (num, ty, name) h _ = Right kit2 where
    code       = C.resIncluded h
    ns         = K.catMaybes [Just num, ty, name]
    kit2       = C.relkitConstBody ns $ map assn code
    assn c     = K.catMaybes [codeNo c, codeType c, codeText c]

    codeNo     = Just         . K.pInt  . K.getIx
    codeType   = maybeAs ty   . K.pText . K.ioPointType . K.nioPoint
    codeText   = maybeAs name . K.pText . K.ioPointText . K.nioPoint

maybeAs :: Maybe a -> b -> Maybe b
maybeAs (Just _)  c  =  Just c
maybeAs (Nothing) _  =  Nothing


-- ----------------------  koshu-angle-text

--  koshu-angle-text /name
--  koshu-angle-text /name /text

consKoshuAngleText :: (Ord c, K.CText c) => C.RopCons c
consKoshuAngleText med =
  do n <- Rop.getTerm med "-name"
     c <- Rop.getMaybe Rop.getTerm med "-text"
     Right $ relmapKoshuAngleText med (n, c)

-- | Create @koshu-angle-text@ relmap.
relmapKoshuAngleText :: (Ord c, K.CText c) => C.Intmed c -> (K.TermName, Maybe K.TermName) -> C.Relmap c
relmapKoshuAngleText med = C.relmapFlow med . relkitKoshuAngleText

-- | Create @koshu-angle-text@ relkit.
relkitKoshuAngleText :: (Ord c, K.CText c) => (K.TermName, Maybe K.TermName) -> Maybe K.Head -> K.Ab (C.Relkit c)
relkitKoshuAngleText (n, Just c) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n, c] assn
    assn (name, text) = [K.pText $ "<" ++ name ++ ">", K.pText $ text]
relkitKoshuAngleText (n, Nothing) _ = Right kit2 where
    kit2 = koshuAngleTextBody [n] assn
    assn (name, _)    = [K.pText $ "<" ++ name ++ ">"]

koshuAngleTextBody :: (Ord c, K.CText c) => [K.TermName] -> ((String, String) -> [c]) -> C.Relkit c
koshuAngleTextBody he assn = C.relkitConstBody he $ K.sort $ map assn K.angleTexts
