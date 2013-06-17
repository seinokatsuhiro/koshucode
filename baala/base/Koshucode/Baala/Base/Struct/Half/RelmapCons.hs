{-# OPTIONS_GHC -Wall #-}

{-| 'Relmap' construction.

    Construction process of half relmaps from source trees.

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

module Koshucode.Baala.Base.Struct.Half.RelmapCons
( relmapCons,
  RelmapCons (..),
  RelmapHalfCons,
  RelmapFullCons
) where

import Koshucode.Baala.Base.Prelude hiding (cat)
import Koshucode.Baala.Base.Struct.Full.HalfRelmap
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.Implement
import Koshucode.Baala.Base.Syntax



-- ----------------------  Constructions

{-| Make half and full relmap constructors. -}
relmapCons
    :: [RelmapImplement v]  -- ^ Implementations of relmap operator
    -> (RelmapCons v)       -- ^ Relmap constructors
relmapCons = make . unzip . map split where
    make (halfs, fulls) =
        RelmapCons (halfBundle halfs) (fullBundle fulls)
    split (RelmapImplement n half full usage) =
        ((n, (usage, half)), (n, full))

{-| Half and full relmap constructor -}
data RelmapCons v = RelmapCons
    { consHalf :: RelmapHalfCons
    , consFull :: RelmapFullCons v
    }

instance Show (RelmapCons v) where
    show _ = "RelmapCons <half> <full>"



-- ----------------------  Half construction

{-| First step of constructing relmap,
    make 'HalfRelmap' from use of relational operator. -}
type RelmapHalfCons
    =  [SourceLine]   -- ^ Source information
    -> [TokenTree]    -- ^ Operand as source trees
    -> HalfRelmap     -- ^ Result half relmap

halfBundle :: [(String, ([String], OperandParser))] -> RelmapHalfCons
halfBundle halfs = consHalfRelmap bundle where
    bundle :: String -> RelmapHalfCons
    bundle op src opd = case lookup op halfs of
      Just (u,p) -> let opd' = addOperand opd $ p opd
                    in HalfRelmap u src op opd' []
      Nothing    -> HalfRelmap [] src op [("operand", opd)] []

    addOperand :: a -> [Named a] -> [Named a]
    addOperand opd = (("operand", opd) :)

consHalfRelmap :: (String -> RelmapHalfCons) -> RelmapHalfCons
consHalfRelmap bundle src = cons where
    cons :: [TokenTree] -> HalfRelmap
    cons xs = case bar xs of
                [x] -> one x
                xs2 -> cat $ map cons xs2

    cons' :: TokenTree -> HalfRelmap
    cons' x = cons [x]

    bar :: [TokenTree] -> [[TokenTree]]
    bar = divideBy $ TreeL (Word 0 "|") -- non-quoted vertical bar

    cat :: [HalfRelmap] -> HalfRelmap
    cat = HalfRelmap ["RELMAP | RELMAP"] src "|" []

    -- half relmap from tokens
    one :: [TokenTree] -> HalfRelmap
    one [TreeB _ xs] = cons xs
    one (TreeL (Word 0 op) : opd) = submap $ bundle op src opd
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
type RelmapFullCons v
    = HalfRelmap
    -> AbortOr (Relmap v) -- ^ Result relmap

{-| Construct (full) relmap. -}
fullBundle :: [Named (OperatorCons v)] -> RelmapFullCons v
fullBundle fulls = full where
    full h@(HalfRelmap u src op _ hs) =
        case lookup op fulls of
          Nothing   -> Right $ RelmapName h op
          Just cons -> do ms <- mapM full hs
                          addAbort (AbortUsage src u) $ cons ms h

