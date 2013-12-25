{-# OPTIONS_GHC -Wall #-}

{-| Data structures for relation-to-relation mappings -}

module Koshucode.Baala.Core.Relmap.Relmap
( 
  -- * Data
  Relmap (..),

  -- * Append relmaps
  -- $AppendRelmaps

  -- * Selectors
  relmapHalf,
  relmapOpToken,
  relmapSourceList,
  relmapNameList,
  relmapAppendList,

  -- * Linker
  relmapLinker,
) where

import qualified Data.Monoid                            as Monoid
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Relfy      as C



-- ----------------------  Data

{-| Relation-to-relation mapping.
    A 'Relmap' is correspond to a use of relational operator. -}
data Relmap c
    -- | Retrieve a relation from a dataset
    = RelmapSource C.HalfRelmap B.JudgePattern [B.Termname]
    -- | Constant relation
    | RelmapConst  C.HalfRelmap (B.Rel c)
    -- | Equavalent relmap
    | RelmapAlias  C.HalfRelmap (Relmap c)
    -- | Relmap that maps relations to a relation
    | RelmapCalc   C.HalfRelmap (C.RelmapConflRelfy c) [Relmap c]
    -- | Connect two relmaps
    | RelmapAppend (Relmap c) (Relmap c)
    -- | Relmap reference
    | RelmapName   C.HalfRelmap String



-- ----------------------  Instances

instance Show (Relmap c) where
    show = showRelmap

showRelmap :: Relmap c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)  = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)     = "RelmapConst "  ++ show (B.name r) ++ " _"
    sh (RelmapAlias  _ r2)    = "RelmapAlias "  ++ show r2
    sh (RelmapCalc  _ _ rs)   = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs
    sh (RelmapAppend r1 r2)   = "RelmapAppend"  ++ joinSubs [r1, r2]
    sh (RelmapName _ n)       = "RelmapName "   ++ show n

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance Monoid.Monoid (Relmap c) where
    mempty  = RelmapCalc halfid (const C.relfyId) []
    mappend = RelmapAppend

halfid :: C.HalfRelmap
halfid = C.HalfRelmap "id" (B.tokenWord "id") [("operand", [])] []

instance B.Name (Relmap c) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  h _)     = C.halfOpText h
    name (RelmapCalc   h _ _)   = C.halfOpText h
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance B.Pretty (Relmap c) where
    doc (RelmapSource h _ _)   = B.doc h
    doc (RelmapConst  h _)     = B.doc h
    doc (RelmapAlias  h _)     = B.doc h
    doc (RelmapCalc   h _ _)   = B.doc h -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapAppend m1 m2)   = B.docHang (B.doc m1) 2 (docRelmapAppend m2)
    doc (RelmapName   _ n)     = B.doc n

docRelmapAppend :: Relmap c -> B.Doc
docRelmapAppend = B.docv . map pipe . relmapAppendList where
    pipe m = B.doc "|" B.<+> B.doc m



-- ----------------------  Selector

relmapHalf :: Relmap c -> Maybe C.HalfRelmap
relmapHalf = half where
    half (RelmapSource h _ _)   = Just h
    half (RelmapConst  h _)     = Just h
    half (RelmapAlias  h _)     = Just h
    half (RelmapCalc   h _ _)   = Just h
    half (RelmapAppend m1 _)    = relmapHalf m1
    half _                      = Nothing

relmapOpToken :: Relmap c -> B.Token
relmapOpToken = f . relmapHalf where
    f (Nothing) = B.tokenWord "?"
    f (Just h)  = C.halfOpToken h

{-| List of 'RelmapSource' -}
relmapSourceList :: Relmap c -> [Relmap c]
relmapSourceList = relmapList f where
    f m@(RelmapSource _ _ _) = [m]
    f _ = []

{-| List of name in 'RelmapName' -}
relmapNameList :: Relmap c -> [String]
relmapNameList = relmapList f where
    f (RelmapName _ n) = [n]
    f _ = []

relmapList :: B.Map (Relmap c -> [a])
relmapList f = loop where
    loop (RelmapAlias _ m1)   = loop m1
    loop (RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop (RelmapCalc _ _ ms)  = concatMap loop ms
    loop m = f m

{-| Expand 'RelmapAppend' to list of 'Relmap' -}
relmapAppendList :: Relmap c -> [Relmap c]
relmapAppendList = loop where
    loop (RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop m = [m]



-- ----------------------  Link

{-| Link relmaps by its name. -}
relmapLinker
    :: [B.Named (Relmap c)]  -- ^ Relmap and its linking name
    -> Relmap c              -- ^ Relmap before linking
    -> Relmap c              -- ^ Linked relmap
relmapLinker = relmapLinker' . relmapLink

relmapLinker' :: [B.Named (Relmap c)] -> B.Map (Relmap c)
relmapLinker' ms' = link where
    link (RelmapAlias h m)     = RelmapAlias h (link m)
    link (RelmapAppend m1 m2)  = RelmapAppend (link m1) (link m2)
    link (RelmapCalc h g ms) = RelmapCalc h g $ map link ms
    link m@(RelmapName _ n)    = case lookup n ms' of
                                   Just m' -> m'
                                   Nothing -> m
    link m                     = m

relmapLink :: B.Map [B.Named (Relmap c)]
relmapLink ms = ms' where
    ms'         = map link ms  -- make linked relmaps
    link (n, m) = (n, linker m)
    linker      = relmapLinker' ms'



-- ----------------------
{- $AppendRelmaps

   This picture represents calculation
   of mapping input relation to output relation.

   > input -[ relmap ]- output

   Relmap /A/ maps relation /R1/ to relation /R2/.
   Another relmap /B/ maps /R2/ to /R3/.

   > R1 -[ A ]- R2
   > R2 -[ B ]- R3

   Two relmaps /A/ and /B/ are jointed
   with intermidiate relation /R2/.

   > R1 -[ A ]- R2 -[ B ]- R3

   Or, we can draw a directly jointed picture.
   This whole structure is also 'RelmapAppend' /A B/.

   > R1 -[ A ]--[ B ]- R3

-}

