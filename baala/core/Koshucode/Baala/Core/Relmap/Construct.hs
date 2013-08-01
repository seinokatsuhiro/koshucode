{-# OPTIONS_GHC -Wall #-}

{-| 'Relmap' construction. -}

module Koshucode.Baala.Core.Relmap.Construct
( relmapCons,
  RelmapCons (..),
  RelmapHalfCons,
  RelmapFullCons

  -- * Construction process
  -- $ConstructionProcess
) where

import Koshucode.Baala.Base hiding (cat)

import Koshucode.Baala.Core.Relmap.HalfRelmap
import Koshucode.Baala.Core.Relmap.Implement
import Koshucode.Baala.Core.Relmap.Relmap



-- ----------------------  Constructions

{-| Make half and full relmap constructors. -}
relmapCons
    :: [Rop c]   -- ^ Implementations of relational operators
    -> (RelmapCons c)     -- ^ Relmap constructors
relmapCons = make . unzip . map split where
    make (halfs, fulls) =
        RelmapCons (halfBundle halfs) (fullBundle fulls)
    split (Rop n _ half full usage) =
        ((n, (usage, half)), (n, full))

{-| Half and full relmap constructor -}
data RelmapCons c = RelmapCons
    { consHalf :: RelmapHalfCons
    , consFull :: RelmapFullCons c
    }

instance Show (RelmapCons c) where
    show _ = "RelmapCons <half> <full>"



-- ----------------------  Half construction

{-| First step of constructing relmap,
    make 'HalfRelmap' from use of relational operator. -}
type RelmapHalfCons
    =  [CodeLine]     -- ^ Source information
    -> [TokenTree]    -- ^ Operand as source trees
    -> HalfRelmap     -- ^ Result half relmap

halfBundle :: [(String, ([String], RopParser))] -> RelmapHalfCons
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
    bar = divideByP isBar  -- non-quoted vertical bar

    isBar (TreeL (TWord _ 0 "|")) = True
    isBar _                       = False

    cat :: [HalfRelmap] -> HalfRelmap
    cat = HalfRelmap ["RELMAP | RELMAP"] src "|" []

    -- half relmap from tokens
    one :: [TokenTree] -> HalfRelmap
    one [TreeB _ xs] = cons xs
    one (TreeL (TWord _ 0 op) : opd) = submap $ bundle op src opd
    one opd = HalfRelmap [] src "?" [("operand", opd)] [] -- no operator

    -- collect subrelmaps
    submap :: HalfRelmap -> HalfRelmap
    submap h@(HalfRelmap u _ op opd _) =
        case lookup "-relmap" opd of
          Just xs -> HalfRelmap u src op opd $ map cons' xs
          Nothing -> h  -- no subrelmaps



-- ----------------------  Full construction

{-| Second step of constructing relmap,
    make 'Relmap' from contents of 'HalfRelmap'. -}
type RelmapFullCons c
    = HalfRelmap          -- ^ Half relmap from 'RelmapHalfCons'
    -> AbortOr (Relmap c) -- ^ Result relmap

{-| Construct (full) relmap. -}
fullBundle :: [Named (RopCons c)] -> RelmapFullCons c
fullBundle fulls = full where
    full h@(HalfRelmap u src op _ hs) =
        case lookup op fulls of
          Nothing   -> Right $ RelmapName h op
          Just cons -> do ms <- mapM full hs
                          addAbort (AbortUsage op u, src)
                             $ cons $ RopUse h ms



-- ----------------------
-- $ConstructionProcess
--
-- Construction process of half relmaps from source trees.
--
-- [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--    Dicide list of 'TokenTree' by vertical bar (@|@).
--
-- [@\[\[TokenTree\]\] -> \[HalfRelmap\]@]
--    Construct each 'HalfRelmap' from lists of 'TokenTree'.
--    When there are subrelmaps in token trees,
--    constructs 'HalfRelmap' recursively.
--
-- [@\[HalfRelmap\] -> HalfRelmap@]
--    Wrap list of 'HalfRelmap' into one 'HalfRelmap'
--    that has these relmaps in 'halfSubmap'.

