{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Struct.Half.HalfRelmap
( -- * Relmap implementation
  makeConsRelmap
, ConsRelmap (..)
, RelmapImplement (..)
, OperandParser
  -- * Half constructor
, RelmapHalfCons
, consHalfRelmap
  -- * Full constructor
, RelmapWholeCons
, RelmapFullCons
) where
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Prelude.Abort
import Koshucode.Baala.Base.Prelude



-- ----------------------  Relmap implementation

-- | Make half and full relmap constructors.
makeConsRelmap
    :: [RelmapImplement v]  -- ^ Implementations of relmap operator
    -> (ConsRelmap v)       -- ^ Relmap constructors
makeConsRelmap = make . unzip . map split where
    split (RelmapImplement n half full usage) =
        ((n, (usage, half)), (n, full))
    make (halfs, fulls) = let half = makeRelmapHalfCons halfs
                              full = makeConsFullRelmap fulls
                          in ConsRelmap half full

-- | Half and full relmap constructor
data ConsRelmap v = ConsRelmap
    { consHalf :: RelmapHalfCons
    , consFull :: RelmapWholeCons v
    }

instance Show (ConsRelmap v) where
    show _ = "ConsRelmap half full"

-- | Implementation of relmap operator.
--   It consists of (1) operator name,
--   (2) operand parser, (3) full constructor,
--   and (4) usage of operator.
data RelmapImplement v = RelmapImplement
    String              -- Operator name
    OperandParser       -- Operand parser
    (RelmapFullCons v)  -- Relmap full constructor
    [String]            -- Description of operator usage

-- | Parser for operand of relational operator.
--   This parsers docompose operand trees,
--   and give a name to suboperand.
type OperandParser = [TokenTree] -> [Named [TokenTree]]



-- ----------------------  Half construction

-- | First step of relmap construction,
--   make 'HalfRelmap' from use of relational operator.
type RelmapHalfCons
    =  String         -- ^ Operator name
    -> [SourceLine]
    -> [TokenTree]    -- ^ Operand as source trees
    -> HalfRelmap     -- ^ Result half relmap with empty subs

makeRelmapHalfCons
    :: [(String, ([String], [TokenTree] -> [Named [TokenTree]]))]
       -- ^ [(op, (usage, half))]
    -> RelmapHalfCons
makeRelmapHalfCons halfs op src opd =
    case lookup op halfs of
      Just (usage, half) -> HalfRelmap usage src op (addOperand opd $ half opd) []
      Nothing -> HalfRelmap [] src op [("operand", opd)] []

addOperand :: a -> [(Named a)] -> [(Named a)]
addOperand opd = (("operand", opd) :)

-- | Construct half relmaps from source trees.
consHalfRelmap :: RelmapHalfCons -> [SourceLine] -> [TokenTree] -> HalfRelmap
consHalfRelmap half src = make where
    make :: [TokenTree] -> HalfRelmap
    make xs = case divideBy bar xs of
                [x] -> one x
                xs2 -> halfAppend $ map make xs2

    make' :: TokenTree -> HalfRelmap
    make' x = make [x]

    bar :: TokenTree
    bar = Bloom $ Word 0 "|"  -- non-quoted vertical bar

    one :: [TokenTree] -> HalfRelmap
    one [Branch _ xs] = make xs
    one (Bloom (Word 0 op) : opd) = sub $ half op src opd
    one opd = HalfRelmap [] src "?" [("operand", opd)] [] -- no operator

    -- collect subrelmaps
    sub :: HalfRelmap -> HalfRelmap
    sub h@(HalfRelmap usage _ op opd _) =
        case lookup "relmap" opd of
          Just xs -> HalfRelmap usage src op opd $ map make' xs
          Nothing -> h  -- no subrelmaps

halfAppend :: [HalfRelmap] -> HalfRelmap
halfAppend = HalfRelmap ["RELMAP | RELMAP"] [] "|" []



-- ----------------------  Full construction

-- | Second step of relmap synthesis,
--   make 'Relmap' from contents of 'HalfRelmap'.
type RelmapWholeCons v
    = HalfRelmap
    -> AbortOr (Relmap v) -- ^ Result relmap

type RelmapFullCons v
    =  [Relmap v]         -- ^ Subrelmaps computed by 'consFullRelmap'
    -> HalfRelmap
    -> AbortOr (Relmap v) -- ^ Result relmap

-- | Construct (full) relmap.
makeConsFullRelmap :: [Named (RelmapFullCons v)] -> RelmapWholeCons v
makeConsFullRelmap fullmap = whole where
    whole h@(HalfRelmap u src op _ hs) =
        case lookup op fullmap of
          Nothing   -> Right $ RelmapName h op
          Just full -> do ms <- mapM whole hs
                          addAbort (AbortUsage src u) $ full ms h

