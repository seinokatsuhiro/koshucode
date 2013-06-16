{-# OPTIONS_GHC -Wall #-}

{-| Intermediate data between 'String' and 'Relmap'. -}

module Koshucode.Baala.Base.Struct.Half.HalfRelmap
( -- * Relmap implementation
  makeRelmapCons
, RelmapCons (..)
, RelmapImplement (..)
, OperandParser

  -- * Half constructor
, RelmapHalfCons
, consHalfRelmap

  -- * Full constructor
, RelmapWholeCons
, RelmapFullCons
) where

import Koshucode.Baala.Base.Prelude hiding (cat)
import Koshucode.Baala.Base.Struct.Full.HalfRelmap
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Syntax



-- ----------------------  Relmap implementation

-- | Make half and full relmap constructors.
makeRelmapCons
    :: [RelmapImplement v]  -- ^ Implementations of relmap operator
    -> (RelmapCons v)       -- ^ Relmap constructors
makeRelmapCons = make . unzip . map split where
    split (RelmapImplement n half full usage) =
        ((n, (usage, half)), (n, full))
    make (halfs, fulls) = let half = makeRelmapHalfCons halfs
                              full = makeConsFullRelmap fulls
                          in RelmapCons half full

-- | Half and full relmap constructor
data RelmapCons v = RelmapCons
    { consHalf :: RelmapHalfCons
    , consFull :: RelmapWholeCons v
    }

instance Show (RelmapCons v) where
    show _ = "RelmapCons <half> <full>"

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
    -> [SourceLine]   -- ^ Source information
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

{-| Construct half relmaps from source trees.

    [@\[TokenTree\] -> \[\[TokenTree\]\]@]
       Divide list of 'TokenTree' by vertical bar (@|@).

    [@\[\[TokenTree\]\] -> \[HalfRelmap\]@]
       Construct each 'HalfRelmap' from lists of 'TokenTree'.
       When there are subrelmaps in token trees,
       constructs 'HalfRelmap' recursively.

    [@\[HalfRelmap\] -> HalfRelmap@]
       Wrap list of 'HalfRelmap' into one 'HalfRelmap'
       that has these relmaps in 'halfSubmap'.
 -}
consHalfRelmap :: RelmapHalfCons -> [SourceLine] -> [TokenTree] -> HalfRelmap
consHalfRelmap half src = cons where
    cons' :: TokenTree -> HalfRelmap
    cons' x = cons [x]

    cons :: [TokenTree] -> HalfRelmap
    cons xs = case bar xs of
                [x] -> one x
                xs2 -> cat $ map cons xs2

    bar :: [TokenTree] -> [[TokenTree]]
    bar = divideBy $ TreeL (Word 0 "|") -- non-quoted vertical bar

    cat :: [HalfRelmap] -> HalfRelmap
    cat = HalfRelmap ["RELMAP | RELMAP"] src "|" []

    -- half relmap from tokens
    one :: [TokenTree] -> HalfRelmap
    one [TreeB _ xs] = cons xs
    one (TreeL (Word 0 op) : opd) = submap $ half op src opd
    one opd = HalfRelmap [] src "?" [("operand", opd)] [] -- no operator

    -- collect subrelmaps
    submap :: HalfRelmap -> HalfRelmap
    submap h@(HalfRelmap u _ op opd _) =
        case lookup "relmap" opd of
          Just xs -> HalfRelmap u src op opd $ map cons' xs
          Nothing -> h  -- no subrelmaps



-- ----------------------  Full construction

{-| Second step of constructing relmap,
    make 'Relmap' from contents of 'HalfRelmap'. -}
type RelmapWholeCons v
    = HalfRelmap
    -> AbortOr (Relmap v) -- ^ Result relmap

type RelmapFullCons v
    =  [Relmap v]         -- ^ Subrelmaps computed by 'consFullRelmap'
    -> HalfRelmap         -- ^ Half-constructed relmap
    -> AbortOr (Relmap v) -- ^ Result relmap

{-| Construct (full) relmap. -}
makeConsFullRelmap :: [Named (RelmapFullCons v)] -> RelmapWholeCons v
makeConsFullRelmap fullmap = whole where
    whole h@(HalfRelmap u src op _ hs) =
        case lookup op fullmap of
          Nothing   -> Right $ RelmapName h op
          Just full -> do ms <- mapM whole hs
                          addAbort (AbortUsage src u) $ full ms h

