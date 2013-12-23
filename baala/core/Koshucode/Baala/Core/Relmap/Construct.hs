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

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Relmap     as C
import qualified Koshucode.Baala.Core.Relmap.Rop        as C



-- ----------------------  Constructions

{-| Make half and full relmap constructors. -}
relmapCons
    :: [C.Rop c]        -- ^ Implementations of relational operators
    -> (RelmapCons c)   -- ^ Relmap constructors
relmapCons = make . unzip . map pair where
    make (halfs, fulls) =
        RelmapCons (halfBundle halfs) (fullBundle fulls)
    pair (C.Rop op _ sorter cons synopsis) =
        ((op, (synopsis, sorter)),
         (op, cons))

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
    =  [B.TokenLine]      -- ^ Source information
    -> [B.TokenTree]      -- ^ Operand as source trees
    -> B.Ab C.HalfRelmap  -- ^ Result half relmap

halfBundle :: [B.Named (String, C.RopFullSorter)] -> RelmapHalfCons
halfBundle operators src = cons where
    cons :: [B.TokenTree] -> B.Ab C.HalfRelmap
    cons xs =
        case B.divideTreesByBar xs of
          [(B.TreeL (B.TWord _ 0 op) : opd)] -> find op opd
          [[B.TreeB 1 xs2]] -> cons xs2
          [[B.TreeB _ _]]   -> Left $ B.AbortAnalysis [] $ B.AAUndefined "bracket"
          [_]               -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap "?"
          xs2               -> find "|" $ map B.treeWrap xs2

    find :: String -> [B.TokenTree] -> B.Ab C.HalfRelmap
    find op opd =
        case lookup op operators of
          Nothing -> Right $ half "" op opd []
          Just (u, sorter) ->
              do sorted <- sorter opd
                 submap $ half u op opd sorted

    half :: String -> String -> [B.TokenTree] -> [B.Named [B.TokenTree]] -> C.HalfRelmap
    half usage op orig opd =
        C.HalfRelmap usage src op (("operand", orig) : opd) []

    submap :: C.HalfRelmap -> B.Ab C.HalfRelmap
    submap h@C.HalfRelmap { C.halfOperand = opd } =
        case lookup "-relmap" opd of
          Nothing -> Right h   -- no subrelmaps
          Just xs -> do subs <- mapM (cons . B.singleton) xs
                        Right $ h { C.halfSubmap = subs }



-- ----------------------  Full construction

{-| Second step of constructing relmap,
    make 'Relmap' from contents of 'HalfRelmap'. -}
type RelmapFullCons c
    = C.HalfRelmap        -- ^ Half relmap from 'RelmapHalfCons'
    -> B.Ab (C.Relmap c)  -- ^ Result relmap

{-| Construct (full) relmap. -}
fullBundle :: [B.Named (C.RopCons c)] -> RelmapFullCons c
fullBundle assoc = c where
    c half@C.HalfRelmap { C.halfOperator = op, C.halfSubmap = hs } =
        case lookup op assoc of
          Nothing   -> Right $ C.RelmapName half op
          Just cons -> do submaps <- mapM c hs
                          cons $ C.RopUse half submaps



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

