{-# OPTIONS_GHC -Wall #-}

-- | Implementation of relmap operators.

module Koshucode.Baala.Core.Relmap.Relmap
  ( -- * Relmap
    Relmap' (..),
    relmapId,
    relmapLexmaps,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C


-- ----------------------  Relmap

-- | Generic relmap.
data Relmap' h c
    = RelmapSource  C.Lexmap B.JudgePat [B.TermName]
                    -- ^ Retrieve a relation from a dataset
    | RelmapConst   C.Lexmap (B.Rel c)
                    -- ^ Constant relation
    | RelmapHook    C.Lexmap (C.RelkitHook' h c)
                    -- ^ Relmap that maps relations to a relation with hook data
    | RelmapCalc    C.Lexmap (C.RelkitConfl c) [Relmap' h c]
                    -- ^ Relmap that maps relations to a relation
    | RelmapCopy    C.Lexmap String (Relmap' h c)
                    -- ^ Relmap for environment of input relation
    | RelmapNest    C.Lexmap [B.Terminal String] (Relmap' h c)
                    -- ^ Relmap for environment of nested relations
    | RelmapLink    C.Lexmap
                    -- ^ Relmap reference
    | RelmapAppend  (Relmap' h c) (Relmap' h c)
                    -- ^ Connect two relmaps

instance Show (Relmap' h c) where
    show = showRelmap

showRelmap :: Relmap' h c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)   = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)      = "RelmapConst "  ++ show (B.name r) ++ " _"

    sh (RelmapHook   _ _)      = "RelmapHook "   ++ show (B.name r)
    sh (RelmapCalc   _ _ rs)   = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs

    sh (RelmapCopy   _ n r1)   = "RelmapCopy "   ++ show n  ++ joinSubs [r1]
    sh (RelmapNest   _ ns r1)  = "RelmapNest "   ++ show ns ++ joinSubs [r1]
    sh (RelmapLink   lx)       = "RelmapLink "   ++ show (C.lexRopName lx)
    sh (RelmapAppend r1 r2)    = "RelmapAppend"  ++ joinSubs [r1, r2]

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance B.Monoid (Relmap' h c) where
    mempty  = relmapId
    mappend = RelmapAppend

instance B.Name (Relmap' h c) where
    name (RelmapSource _ _ _)       = "source"
    name (RelmapConst  lx _)        = C.lexRopName lx
    name (RelmapCalc   lx _ _)      = C.lexRopName lx
    name (RelmapAppend _ _)         = "append"
    name _ = undefined

instance B.Write (Relmap' h c) where
    write sh (RelmapSource lx _ _)  = B.write sh lx
    write sh (RelmapConst  lx _)    = B.write sh lx

    write sh (RelmapHook   lx _)    = B.write sh lx -- hang (text $ name m) 2 (writeh (map write ms))
    write sh (RelmapCalc   lx _ _)  = B.write sh lx -- hang (text $ name m) 2 (writeh (map write ms))

    write sh (RelmapCopy   _ _ r1)  = B.write sh r1
    write sh (RelmapNest   _ _ r1)  = B.write sh r1
    write sh (RelmapLink   lx)      = B.write sh lx
    write sh (RelmapAppend r1 r2)   = B.docHang (B.write sh r1) 2 (docRelmapAppend sh r2)

docRelmapAppend :: B.StringMap -> Relmap' h c -> B.Doc
docRelmapAppend sh = B.writeV sh . map pipe . relmapAppendList where
    pipe m = B.write sh "|" B.<+> B.write sh m

-- | Expand 'RelmapAppend' to list of 'Relmap'
relmapAppendList :: Relmap' h c -> [Relmap' h c]
relmapAppendList = expand where
    expand (RelmapAppend r1 r2) = expand r1 ++ expand r2
    expand r = [r]

instance B.CodePtr (Relmap' h c) where
    codePtList = concatMap B.codePtList . relmapLexmaps

instance Ord (Relmap' h c) where
    r1 `compare` r2  = relmapLexmaps r1 `compare` relmapLexmaps r2

instance Eq (Relmap' h c) where
    r1 == r2         = compare r1 r2 == EQ

-- | Identity relmap.
relmapId :: Relmap' h c
relmapId = RelmapCalc lexId (const $ Right . C.relkitId) []

lexId :: C.Lexmap
lexId = C.lexBase { C.lexRopToken = B.textToken "id" }

relmapLexmaps :: Relmap' h c -> [C.Lexmap]
relmapLexmaps = collect where
    collect (RelmapSource  lx _ _)   = [lx]
    collect (RelmapConst   lx _)     = [lx]

    collect (RelmapHook    lx _)     = [lx]
    collect (RelmapCalc    lx _ _)   = [lx]

    collect (RelmapCopy    lx _ _)   = [lx]
    collect (RelmapNest    lx _ _)   = [lx]
    collect (RelmapLink    lx)       = [lx]
    collect (RelmapAppend  r1 r2)    = collect r1 ++ collect r2

