{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Data structures for relation-to-relation mappings

module Koshucode.Baala.Base.Struct.Full.Relmap
( -- * Relmap
  Relmap (..)
, RelmapSub
, RelmapBin
, relmapSource
, relmapSourceList
, relmapAppendList
, relmapCalc
, relmapConfl
, flow
  -- * Half
, HalfRelmap (..)
  -- * Run
, runRelmap
) where
import Data.Generics
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Data.Monoid



-- ----------------------  Relmap

-- | Relation-to-relation mapping.
--   A Relmap is correspond to a pair of operator and its operands,
--   that is a use of relational operator.
--   Calculations for relations are done by 'runRelmap'.
-- 
--   This picture represents calculation
--   of mapping input relation to output relation.
-- 
--   @   input -[ relmap ]- output   @
-- 
--   Relmap /A/ maps relation /R1/ to relation /R2/.
--   Another relmap /B/ maps /R2/ to /R3/.
-- 
--   @   R1 -[ A ]- R2
--   R2 -[ B ]- R3   @
-- 
--   Two relmaps /A/ and /B/ are jointed
--   with intermidiate relation /R2/.
--   
--   @   R1 -[ A ]- R2 -[ B ]- R3   @
-- 
--   Or, we can draw a directly jointed picture.
--   This whole structure is also 'Relmap' of 'RelmapAppend' /A B/.
-- 
--   @   R1 -[ A ]--[ B ]- R3   @

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

-- | Retrieve relation from dataset
relmapSource :: HalfRelmap -> String -> [String] -> (Relmap v)
relmapSource = RelmapSource

-- | Make a non-confluent relmap
relmapCalc :: HalfRelmap -> String -> RelmapSub v -> Relmap v
relmapCalc h op sub = RelmapCalc h op sub []

-- | Make a confluent relmap
relmapConfl :: HalfRelmap -> String -> RelmapSub v -> [Relmap v] -> Relmap v
relmapConfl = RelmapCalc

-- | Make a flow relmap
flow :: String -> RelmapFun v -> Relmap v
flow = undefined -- RelmapFlow



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



-- ----------------------  Half

-- | Intermediate data that represents use of relational operator.
-- 
--   'HalfRelmap' is constructed from list of 'TokenTree',
--   and (full) 'Relmap' is constructed from 'HalfRelmap'.

data HalfRelmap = HalfRelmap
    { halfUsage    :: [String]      -- ^ Usages description
    , halfLines    :: [SourceLine]  -- ^ Source information
    , halfOperator :: String        -- ^ Operator name of relmap operation
    , halfOperand  :: [Named [TokenTree]] -- ^ Operand of relmap operation
    , halfSubmap   :: [HalfRelmap]        -- ^ Subrelmaps in the operand
    } deriving (Show, Data, Typeable)

instance Pretty HalfRelmap where
    doc HalfRelmap { halfOperator = op, halfOperand = opd } =
        case lookup "operand" opd of
          Nothing -> text op <+> text "..."
          Just xs -> text op <+> text (tokenTreesSource xs)



-- ----------------------  Run

-- | Calculate 'Relmap' for 'Rel'.
runRelmap
    :: (Ord v, Nil v)
    => Dataset v        -- ^ Judges read from @source@ operator
    -> Relmap v         -- ^ Mapping from 'Rel' to 'Rel'
    -> Rel v            -- ^ Input relation
    -> AbortOr (Rel v)  -- ^ Output relation
runRelmap ds = (<$>) where
    RelmapSource _ s ns <$> _ = Right (selectRelation ds s ns)
    RelmapConst _ _ r   <$> _ = Right r
    RelmapAlias _ m     <$> r = m <$> r
    RelmapCalc _ _ f ms <$> r = do rs' <- mapM (<$> reldee) ms
                                   Right $ f rs' r
    RelmapAppend m1 m2  <$> r = do r' <- m1 <$> r
                                   m2 <$> r'
    RelmapName h op     <$> _ = Left $ AbortUnknownRelmap (halfLines h) op

