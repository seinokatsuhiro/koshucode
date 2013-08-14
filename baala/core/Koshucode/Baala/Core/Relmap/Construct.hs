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

import qualified Koshucode.Baala.Base as B

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
    =  [B.TokenLine]    -- ^ Source information
    -> [B.TokenTree]    -- ^ Operand as source trees
    -> HalfRelmap       -- ^ Result half relmap

halfBundle :: [(String, ([String], RopParser))] -> RelmapHalfCons
halfBundle halfs = consHalfRelmap bundle where
    bundle :: String -> RelmapHalfCons
    bundle op src opd = case lookup op halfs of
      Just (u,p) -> let opd' = addOperand opd $ p opd
                    in HalfRelmap u src op opd' []
      Nothing    -> HalfRelmap [] src op [("operand", opd)] []

    addOperand :: a -> [B.Named a] -> [B.Named a]
    addOperand opd = (("operand", opd) :)

consHalfRelmap :: (String -> RelmapHalfCons) -> RelmapHalfCons
consHalfRelmap bundle src = cons where
    cons :: [B.TokenTree] -> HalfRelmap
    cons xs = case bar xs of
                [x] -> one x
                xs2 -> cat $ map cons xs2

    cons' :: B.TokenTree -> HalfRelmap
    cons' x = cons [x]

    bar :: [B.TokenTree] -> [[B.TokenTree]]
    bar = B.divideByP isBar  -- non-quoted vertical bar

    isBar (B.TreeL (B.TWord _ 0 "|")) = True
    isBar _                           = False

    cat :: [HalfRelmap] -> HalfRelmap
    cat = HalfRelmap ["RELMAP | RELMAP"] src "|" []

    -- half relmap from tokens
    one :: [B.TokenTree] -> HalfRelmap
    one [B.TreeB _ xs] = cons xs
    one (B.TreeL (B.TWord _ 0 op) : opd) = submap $ bundle op src opd
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
    = HalfRelmap                -- ^ Half relmap from 'RelmapHalfCons'
    -> B.AbortTokens (Relmap c) -- ^ Result relmap

{-| Construct (full) relmap. -}
fullBundle :: [B.Named (RopCons c)] -> RelmapFullCons c
fullBundle fulls = full where
    full h@(HalfRelmap _ _ op _ hs) =
        case lookup op fulls of
          Nothing   -> Right $ RelmapName h op
          Just cons -> do ms <- mapM full hs
                          --B.addAbort (B.AbortUsage op u, [], src)
                          cons $ RopUse h ms



-- ----------------------
{- $ConstructionProcess
  
   Construction process of half relmaps from source trees.
  
   [@\[TokenTree\] -> \[\[TokenTree\]\]@]
      Dicide list of 'TokenTree' by vertical bar (@|@).
  
   [@\[\[TokenTree\]\] -> \[HalfRelmap\]@]
      Construct each 'HalfRelmap' from lists of 'TokenTree'.
      When there are subrelmaps in token trees,
      constructs 'HalfRelmap' recursively.
  
   [@\[HalfRelmap\] -> HalfRelmap@]
      Wrap list of 'HalfRelmap' into one 'HalfRelmap'
      that has these relmaps in 'halfSubmap'.
-}

