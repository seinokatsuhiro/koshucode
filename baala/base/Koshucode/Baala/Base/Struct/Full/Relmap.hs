{-# OPTIONS_GHC -Wall #-}

-- | Data structures for relation-to-relation mappings

module Koshucode.Baala.Base.Struct.Full.Relmap
( 
  -- * Data
  Relmap (..)

  -- ** Function type
, RelmapSub
, RelmapBin

  -- ** Append relmaps
  -- $AppendRelmaps

  -- * Constructor
, relmapSource
, relmapCalc
, relmapConfl

  -- * Selector
, relmapSourceList
, relmapAppendList
) where

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Struct.Full.HalfRelmap
import Data.Monoid



-- ----------------------  Data

{-| Relation-to-relation mapping.
    A 'Relmap' is correspond to a use of relational operator,
    that is pair of operator and its operand. -}
data Relmap v
    -- | Relmap that retrieves a relation from a dataset
    = RelmapSource HalfRelmap String [String]
    -- | Relmap that has a constant relation
    | RelmapConst  HalfRelmap String (Rel v)
    -- | Relmap that has equavalent relmap
    | RelmapAlias  HalfRelmap (Relmap v)
    -- | Relmap that maps relations to a relation
    | RelmapCalc   HalfRelmap String (RelmapSub v) [Relmap v]
    -- | Relmap that joints two relmaps
    | RelmapAppend (Relmap v) (Relmap v)
    -- | Relmap reference
    | RelmapName   HalfRelmap String

-- | Function of relmap
type RelmapSub v = [Rel v] -> Rel v -> Rel v

-- | Confluent operator, like binary operator
type RelmapBin v = Relmap v -> Relmap v



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



-- ----------------------  Constructor

-- | Retrieve relation from dataset
relmapSource :: HalfRelmap -> String -> [String] -> (Relmap v)
relmapSource = RelmapSource

-- | Make a non-confluent relmap
relmapCalc :: HalfRelmap -> String -> RelmapSub v -> Relmap v
relmapCalc h op sub = RelmapCalc h op sub []

-- | Make a confluent relmap
relmapConfl :: HalfRelmap -> String -> RelmapSub v -> [Relmap v] -> Relmap v
relmapConfl = RelmapCalc



-- ----------------------  Selector

-- | List of 'RelmapSource'
relmapSourceList :: Relmap v -> [Relmap v]
relmapSourceList = loop where
    loop m@(RelmapSource _ _ _) = [m]
    loop (RelmapConst _ _ _)    = []
    loop (RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop (RelmapCalc _ _ _ ms)  = concatMap loop ms
    loop _ = undefined

-- | Expand 'RelmapAppend' to list of 'Relmap'
relmapAppendList :: Relmap v -> [Relmap v]
relmapAppendList = loop where
    loop (RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop m = [m]


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

