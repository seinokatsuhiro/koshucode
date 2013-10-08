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
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Rop        as C
import qualified Koshucode.Baala.Core.Relmap.Relmap     as C



-- ----------------------  Constructions

{-| Make half and full relmap constructors. -}
relmapCons
    :: [C.Rop c]        -- ^ Implementations of relational operators
    -> (RelmapCons c)   -- ^ Relmap constructors
relmapCons = make . unzip . map split where
    make (halfs, fulls) =
        RelmapCons (halfBundle halfs) (fullBundle fulls)
    split (C.Rop n _ half full usage) =
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
    -> C.HalfRelmap     -- ^ Result half relmap

halfBundle :: [(String, (String, C.RopFullSorter))] -> RelmapHalfCons
halfBundle halfs = consHalfRelmap bundle where
    bundle :: String -> RelmapHalfCons
    bundle op src opd = case lookup op halfs of
      Just (u, p) -> let opd' = addOperand opd $ p opd
                     in C.HalfRelmap u src op opd' []
      Nothing     -> C.HalfRelmap [] src op [("operand", opd)] []

    addOperand :: a -> [B.Named a] -> [B.Named a]
    addOperand opd = (("operand", opd) :)

consHalfRelmap :: (String -> RelmapHalfCons) -> RelmapHalfCons
consHalfRelmap bundle src = cons where
    cons :: [B.TokenTree] -> C.HalfRelmap
    cons xs = case B.divideTreesByBar xs of
                [x] -> one x
                xs2 -> cat $ map cons xs2

    cons' :: B.TokenTree -> C.HalfRelmap
    cons' x = cons [x]

    cat :: [C.HalfRelmap] -> C.HalfRelmap
    cat = C.HalfRelmap "R | R" src "|" []

    -- half relmap from tokens
    one :: [B.TokenTree] -> C.HalfRelmap
    one [B.TreeB _ xs] = cons xs
    one (B.TreeL (B.TWord _ 0 op) : opd) = submap $ bundle op src opd
    one opd = C.HalfRelmap "" src "?" [("operand", opd)] [] -- no operator

    -- collect subrelmaps
    submap :: B.Map C.HalfRelmap
    submap h@(C.HalfRelmap u _ op opd _) =
        case lookup "-relmap" opd of
          Just xs -> C.HalfRelmap u src op opd $ map cons' xs
          Nothing -> h  -- no subrelmaps



-- ----------------------  Full construction

{-| Second step of constructing relmap,
    make 'Relmap' from contents of 'HalfRelmap'. -}
type RelmapFullCons c
    = C.HalfRelmap                -- ^ Half relmap from 'RelmapHalfCons'
    -> B.AbortTokens (C.Relmap c) -- ^ Result relmap

{-| Construct (full) relmap. -}
fullBundle :: [B.Named (C.RopCons c)] -> RelmapFullCons c
fullBundle fulls = full where
    full h@(C.HalfRelmap _ _ op _ hs) =
        case lookup op fulls of
          Nothing   -> Right $ C.RelmapName h op
          Just cons -> do ms <- mapM full hs
                          --B.addAbort (B.AbortUsage op u, [], src)
                          cons $ C.RopUse h ms



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

