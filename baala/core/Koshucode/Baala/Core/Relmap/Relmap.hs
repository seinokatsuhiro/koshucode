{-# OPTIONS_GHC -Wall #-}

-- | Generic relmap.

module Koshucode.Baala.Core.Relmap.Relmap
  ( -- * Type for relmap
    Relmap' (..),
    relmapId,

    -- * Select from relmap
    relmapLexmaps,
    relmapSourceList,
    relmapNameList,
  ) where

import qualified Koshucode.Baala.Overture      as O
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Syntax        as S
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Core.Lexmap   as C
import qualified Koshucode.Baala.Core.Relkit   as C


-- ----------------------  Relmap

-- | Generic relmap.
data Relmap' h c
    = RelmapConst   C.Lexmap (D.Rel c)
                             -- ^ Constant relation
    | RelmapSource  C.Lexmap D.JudgeClass [S.TermName]
                             -- ^ Retrieve a relation from a dataset

    | RelmapCalc    C.Lexmap (C.RelkitConfl c) [Relmap' h c]
                             -- ^ Relmap that maps relations to a relation
    | RelmapHook    C.Lexmap (C.RelkitHook' h c)
                             -- ^ Relmap that maps relations to a relation with hook data

    | RelmapCopy    C.Lexmap String (Relmap' h c)
                             -- ^ Relmap for environment of input relation
    | RelmapNest    C.Lexmap (Relmap' h c)
                             -- ^ Relmap for environment of nested relations
    | RelmapLink    C.Lexmap
                             -- ^ Relmap reference
    | RelmapAppend  (Relmap' h c) (Relmap' h c)
                             -- ^ Connect two relmaps

instance Show (Relmap' h c) where
    show = showRelmap

showRelmap :: Relmap' h c -> String
showRelmap r = sh r where
    sh (RelmapConst  _ _)      = "RelmapConst "  ++ show (B.name r) ++ " _"
    sh (RelmapSource _ p xs)   = "RelmapSource " ++ show p ++ " " ++ show xs

    sh (RelmapCalc   _ _ rs)   = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs
    sh (RelmapHook   _ _)      = "RelmapHook "   ++ show (B.name r)

    sh (RelmapCopy   _ n r1)   = "RelmapCopy "   ++ show n  ++ joinSubs [r1]
    sh (RelmapNest   _ r1)     = "RelmapNest "   ++ joinSubs [r1]
    sh (RelmapLink   lx)       = "RelmapLink "   ++ show (C.lexName lx)
    sh (RelmapAppend r1 r2)    = "RelmapAppend"  ++ joinSubs [r1, r2]

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance Ord (Relmap' h c) where
    r1 `compare` r2  = relmapLexmaps r1 `compare` relmapLexmaps r2

instance Eq (Relmap' h c) where
    r1 == r2         = compare r1 r2 == EQ

instance Monoid (Relmap' h c) where
    mempty  = relmapId
    mappend = RelmapAppend

instance B.Name (Relmap' h c) where
    name (RelmapSource _ _ _)       = "source"
    name (RelmapAppend _ _)         = "append"
    name (RelmapConst  lx _)        = C.lexName lx
    name (RelmapCalc   lx _ _)      = C.lexName lx
    name _ = undefined

instance B.CodePtr (Relmap' h c) where
    codePtList = concatMap B.codePtList . relmapLexmaps

-- | Identity relmap.
relmapId :: Relmap' h c
relmapId = RelmapCalc lexId (const $ Right . C.relkitId) []

lexId :: C.Lexmap
lexId = B.def { C.lexToken = S.rawTextToken "id" }


-- ----------------------  Selector

-- | Extract lexmap list from relmap.
relmapLexmaps :: Relmap' h c -> [C.Lexmap]
relmapLexmaps = collect where
    collect (RelmapConst   lx _)     = [lx]
    collect (RelmapSource  lx _ _)   = [lx]

    collect (RelmapCalc    lx _ _)   = [lx]
    collect (RelmapHook    lx _)     = [lx]

    collect (RelmapCopy    lx _ _)   = [lx]
    collect (RelmapNest    lx _)     = [lx]
    collect (RelmapLink    lx)       = [lx]
    collect (RelmapAppend  r1 r2)    = collect r1 ++ collect r2

-- | List of 'C.RelmapSource'
relmapSourceList :: Relmap' h c -> [Relmap' h c]
relmapSourceList = relmapList f where
    f rmap@(RelmapSource _ _ _) = [rmap]
    f _ = []

-- | List of name in 'C.RelmapLink'
relmapNameList :: Relmap' h c -> [String]
relmapNameList = relmapList f where
    f (RelmapLink lx) = [C.lexName lx]
    f _ = []

relmapList :: O.Map (Relmap' h c -> [a])
relmapList f = loop where
    loop (RelmapAppend rmap1 rmap2)  = loop rmap1 ++ loop rmap2
    loop (RelmapCalc _ _ rmaps)      = concatMap loop rmaps
    loop m = f m
