{-# OPTIONS_GHC -Wall #-}

-- | Data structures for relation-to-relation mappings

module Koshucode.Baala.Base.Relmap.Relmap
( 
  -- * Data
  Relmap (..)
, RelmapSub

  -- * Append relmaps
  -- $AppendRelmaps

  -- * Selectors
, relmapSourceList
, relmapNameList
, relmapAppendList

  -- * Linker
, relmapLinker
) where

import Data.Monoid
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Relmap.HalfRelmap



-- ----------------------  Data

{-| Relation-to-relation mapping.
    A 'Relmap' is correspond to a use of relational operator. -}
data Relmap v
    -- | Retrieve a relation from a dataset
    = RelmapSource HalfRelmap String [String]
    -- | Constant relation
    | RelmapConst  HalfRelmap String (Rel v)
    -- | Equavalent relmap
    | RelmapAlias  HalfRelmap (Relmap v)
    -- | Relmap that maps relations to a relation
    | RelmapCalc   HalfRelmap String (RelmapSub v) [Relmap v]
    -- | Connect two relmaps
    | RelmapAppend (Relmap v) (Relmap v)
    -- | Relmap reference
    | RelmapName   HalfRelmap String

{-| Function of relmap. -}
type RelmapSub v
    = [Rel v]   -- ^ Relations in operand
    -> Rel v    -- ^ Main input relation
    -> Rel v    -- ^ Output relation



-- ----------------------  Instances

instance Show (Relmap v) where
    show = showRelmap

showRelmap :: Relmap v -> String
showRelmap = sh where
    sh (RelmapSource _ n xs)     = "RelmapSource " ++ show n ++ " " ++ show xs
    sh (RelmapConst  _ n _)      = "RelmapConst "  ++ show n ++ " _"
    sh (RelmapAlias  _ m)        = "RelmapAlias "  ++ show m
    sh (RelmapCalc   _ n _ subs) = "RelmapCalc "   ++ show n ++ " _" ++ joinSubs subs
    sh (RelmapAppend m1 m2)      = "RelmapAppend"  ++ joinSubs [m1, m2]
    sh (RelmapName _ n)          = "RelmapName "   ++ show n

    joinSubs = concatMap sub
    sub m = " (" ++ sh m ++ ")"

instance Monoid (Relmap v) where
    mempty  = RelmapCalc halfid "id" relid []
    mappend = RelmapAppend

halfid :: HalfRelmap
halfid = HalfRelmap ["id"] [] "id" [("operand", [])] []

relid :: RelmapSub v
relid _ r = r

instance Name (Relmap v) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  _ n _)   = n
    name (RelmapCalc   _ n _ _) = n
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance Pretty (Relmap v) where
    doc (RelmapSource h _ _)   = doc h
    doc (RelmapConst  h _ _)   = doc h
    doc (RelmapAlias  h _)     = doc h
    doc (RelmapCalc   h _ _ _) = doc h -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapAppend m1 m2)   = hang (doc m1) 2 (docRelmapAppend m2)
    doc (RelmapName   _ n)     = text n

docRelmapAppend :: Relmap v -> Doc
docRelmapAppend = docv . map pipe . relmapAppendList where
    pipe m = text "|" <+> doc m



-- ----------------------  Selector

{-| List of 'RelmapSource' -}
relmapSourceList :: Relmap v -> [Relmap v]
relmapSourceList = relmapList f where
    f m@(RelmapSource _ _ _) = [m]
    f _ = []

{-| List of name in 'RelmapName' -}
relmapNameList :: Relmap v -> [String]
relmapNameList = relmapList f where
    f (RelmapName _ n) = [n]
    f _ = []

relmapList :: Map (Relmap v -> [a])
relmapList f = loop where
    loop (RelmapAlias _ m1)     = loop m1
    loop (RelmapAppend m1 m2)   = loop m1 ++ loop m2
    loop (RelmapCalc _ _ _ ms)  = concatMap loop ms
    loop m = f m

{-| Expand 'RelmapAppend' to list of 'Relmap' -}
relmapAppendList :: Relmap v -> [Relmap v]
relmapAppendList = loop where
    loop (RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop m = [m]



-- ----------------------  Link

{-| Link relmaps by its name. -}
relmapLinker
    :: [Named (Relmap v)]  -- ^ Relmap and its linking name
    -> Relmap v            -- ^ Relmap before linking
    -> Relmap v            -- ^ Linked relmap
relmapLinker = relmapLinker' . relmapLink

relmapLinker' :: [Named (Relmap v)] -> Map (Relmap v)
relmapLinker' ms' = link where
    link (RelmapAlias h m)     = RelmapAlias h (link m)
    link (RelmapAppend m1 m2)  = RelmapAppend (link m1) (link m2)
    link (RelmapCalc h n f ms) = RelmapCalc h n f $ map link ms
    link m@(RelmapName _ n)    = case lookup n ms' of
                                   Just m' -> m'
                                   Nothing -> m
    link m                     = m

relmapLink :: Map [Named (Relmap v)]
relmapLink ms = ms' where
    ms'         = map link ms  -- make linked relmaps
    link (n, m) = (n, linker m)
    linker      = relmapLinker' ms'



-- ----------------------
-- $AppendRelmaps
--
-- This picture represents calculation
-- of mapping input relation to output relation.
-- 
-- @ input -[ relmap ]- output @
-- 
-- Relmap /A/ maps relation /R1/ to relation /R2/.
-- Another relmap /B/ maps /R2/ to /R3/.
-- 
-- @ R1 -[ A ]- R2
-- R2 -[ B ]- R3 @
-- 
-- Two relmaps /A/ and /B/ are jointed
-- with intermidiate relation /R2/.
-- 
-- @ R1 -[ A ]- R2 -[ B ]- R3 @
-- 
-- Or, we can draw a directly jointed picture.
-- This whole structure is also 'RelmapAppend' /A B/.
-- 
-- @ R1 -[ A ]--[ B ]- R3 @

