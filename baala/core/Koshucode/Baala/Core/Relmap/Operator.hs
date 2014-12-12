{-# OPTIONS_GHC -Wall #-}

-- | Implementation of relmap operators.

module Koshucode.Baala.Core.Relmap.Operator
  ( -- * Rop
    Global,
    RopUsage,
    Rop (..),
  
    -- * RopUse
    RopCons,
    RopUse (..),
    ropCopset,
  
    -- * Relmap
    Relmap (..),
    relmapId,
    relmapLexList,
  
    -- * Relkit
    RelkitFlow,
    RelkitGlobal,
    RelkitBinary,
    RelkitConfl,
  
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap            as C
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.Global     as C
import qualified Koshucode.Baala.Core.Relmap.Relkit     as C


-- ----------------------  Rop

-- | Global parameters
type Global c = C.Global' Rop c

type RopUsage = String

-- | Implementation of relmap operator
data Rop c = Rop
    { ropName     :: C.RopName      -- ^ Operator name
    , ropGroup    :: String         -- ^ Operator group
    , ropSorter   :: C.AttrSort     -- ^ Attribute sorter
    , ropCons     :: RopCons c      -- ^ Constructor of operator
    , ropUsage    :: RopUsage       -- ^ Usage of operator
    }

instance Show (Rop c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop " ++ group ++ "/" ++ name

instance B.Name (Rop c) where
    name = ropName


-- ----------------------  RopUse

-- | Constructor of relmap operator
type RopCons c = RopUse c -> B.Ab (Relmap c)

-- | Use of relmap operator
data RopUse c = RopUse
    { ropGlobal    :: Global c
    , ropLexmap    :: C.Lexmap     -- ^ Syntactic data of operator use
    , ropSubrelmap :: [Relmap c]   -- ^ Subrelmaps
    } deriving (Show)

instance B.CodePtr (RopUse c) where
    codePts = B.codePts . ropLexmap

-- | Get operator set from 'RopUse'.
ropCopset :: RopUse c -> C.CopSet c
ropCopset = C.globalCopset . ropGlobal



-- ----------------------  Relkit

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c   = Maybe B.Head -> B.Ab (C.Relkit c)

-- | Make 'C.Relkit' from globals and input heading.
type RelkitGlobal c = Global c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c = C.Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c  = [(C.Relkit c)] -> RelkitFlow c


-- ----------------------  Relmap

-- | Generic relmap.
data Relmap c
    = RelmapSource   C.Lexmap B.JudgePat [B.TermName]
      -- ^ Retrieve a relation from a dataset
    | RelmapConst    C.Lexmap (B.Rel c)
      -- ^ Constant relation

    | RelmapGlobal   C.Lexmap (RelkitGlobal c)
      -- ^ Relmap that maps relations to a relation with globals
    | RelmapCalc     C.Lexmap (RelkitConfl c) [Relmap c]
      -- ^ Relmap that maps relations to a relation

    | RelmapCopy     C.Lexmap String (Relmap c)
      -- ^ Relmap for environment of input relation
    | RelmapNest     C.Lexmap [B.Terminal String] (Relmap c)
      -- ^ Relmap for environment of nested relations
    | RelmapLink     C.Lexmap C.RelmapKey
      -- ^ Relmap reference

    | RelmapAppend   (Relmap c) (Relmap c)
      -- ^ Connect two relmaps

instance Show (Relmap c) where
    show = showRelmap

showRelmap :: Relmap c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)  = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)     = "RelmapConst "  ++ show (B.name r) ++ " _"

    sh (RelmapGlobal _ _)     = "RelmapGlobal " ++ show (B.name r)
    sh (RelmapCalc   _ _ rs)  = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs

    sh (RelmapCopy _ n r1)    = "RelmapCopy "   ++ show n  ++ joinSubs [r1]
    sh (RelmapNest _ ns r1)   = "RelmapNest "   ++ show ns ++ joinSubs [r1]
    sh (RelmapLink _ (n, _))  = "RelmapLink "   ++ show n
    sh (RelmapAppend r1 r2)   = "RelmapAppend"  ++ joinSubs [r1, r2]

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance B.Monoid (Relmap c) where
    mempty  = relmapId
    mappend = RelmapAppend

instance B.Name (Relmap c) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  h _)     = C.lexOpName h
    name (RelmapCalc   h _ _)   = C.lexOpName h
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance B.Write (Relmap c) where
    write sh (RelmapSource lx _ _)  = B.write sh lx
    write sh (RelmapConst  lx _)    = B.write sh lx

    write sh (RelmapGlobal lx _)    = B.write sh lx -- hang (text $ name m) 2 (writeh (map write ms))
    write sh (RelmapCalc   lx _ _)  = B.write sh lx -- hang (text $ name m) 2 (writeh (map write ms))

    write sh (RelmapCopy   _ _ r1)  = B.write sh r1
    write sh (RelmapNest   _ _ r1)  = B.write sh r1
    write sh (RelmapLink   lx _)    = B.write sh lx
    write sh (RelmapAppend r1 r2)   = B.docHang (B.write sh r1) 2 (docRelmapAppend sh r2)

docRelmapAppend :: B.StringMap -> Relmap c -> B.Doc
docRelmapAppend sh = B.writeV sh . map pipe . relmapAppendList where
    pipe m = B.write sh "|" B.<+> B.write sh m

-- | Expand 'RelmapAppend' to list of 'Relmap'
relmapAppendList :: Relmap c -> [Relmap c]
relmapAppendList = expand where
    expand (RelmapAppend r1 r2) = expand r1 ++ expand r2
    expand r = [r]

instance B.CodePtr (Relmap c) where
    codePts = concatMap B.codePts . relmapLexList

instance Ord (Relmap c) where
    r1 `compare` r2 = relmapLexList r1 `compare` relmapLexList r2

instance Eq (Relmap c) where
    r1 == r2  =  compare r1 r2 == EQ

-- | Identity relmap.
relmapId :: Relmap c
relmapId = RelmapCalc lexId (const $ Right . C.relkitId) []

lexId :: C.Lexmap
lexId = C.Lexmap C.LexmapBase name attr [] [] where
    name = B.textToken "id"
    attr = [(C.attrNameAttr, [])]

relmapLexList :: Relmap c -> [C.Lexmap]
relmapLexList = collect where
    collect (RelmapSource  lx _ _)  = [lx]
    collect (RelmapConst   lx _)    = [lx]

    collect (RelmapGlobal  lx _)    = [lx]
    collect (RelmapCalc    lx _ _)  = [lx]

    collect (RelmapCopy    lx _ _)  = [lx]
    collect (RelmapNest    lx _ _)  = [lx]
    collect (RelmapLink    lx _)    = [lx]
    collect (RelmapAppend  r1 r2)   = collect r1 ++ collect r2

