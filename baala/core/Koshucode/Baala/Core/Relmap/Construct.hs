{-# OPTIONS_GHC -Wall #-}

{-| 'Relmap' construction. -}

module Koshucode.Baala.Core.Relmap.Construct
( relmapCons,
  RelmapCons (..),
  RelmapConsHalf,
  RelmapConsFull,

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
    :: [C.Rop c]      -- ^ Implementations of relmap operators
    -> (RelmapCons c) -- ^ Relmap constructors
relmapCons = make . unzip . map pair where
    make (halfs, fulls) =
        RelmapCons (makeConsHalf halfs) (makeConsFull fulls)
    pair (C.Rop op _ sorter cons synopsis) =
        let half = (op, (synopsis, sorter))
            full = (op, cons)
        in (half, full)

{-| Half and full relmap constructor -}
data RelmapCons c = RelmapCons RelmapConsHalf (RelmapConsFull c)

instance Show (RelmapCons c) where
    show _ = "RelmapCons <half> <full>"


-- ----------------------  Half construction

{-| First step of constructing relmap,
    make 'C.HalfRelmap' from use of relmap operator. -}
type RelmapConsHalf
    =  [B.TokenTree]      -- ^ Source of relmap operator
    -> B.Ab C.HalfRelmap  -- ^ Result half relmap

makeConsHalf :: [B.Named (String, C.RopFullSorter)] -> RelmapConsHalf
makeConsHalf halfs = consHalf where
    consHalf :: RelmapConsHalf
    consHalf tree =
        case B.divideTreesByBar tree of
          [(B.TreeL tok@(B.TWord _ 0 _) : opd)] -> find tok opd
          [[B.TreeB 1 tree2]] -> consHalf tree2
          [[B.TreeB _ _]]     -> Left $ B.AbortAnalysis [] $ B.AAUndefined "bracket"
          [_]                 -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap "?"
          tree2               -> find (B.tokenWord "|") $ map B.treeWrap tree2

    find :: B.Token -> [B.TokenTree] -> B.Ab C.HalfRelmap
    find tok opd =
        case lookup (B.tokenContent tok) halfs of
          Nothing -> Right $ half "" tok opd []
          Just (usage, sorter) ->
              do sorted <- sorter opd
                 subrelmap $ half usage tok opd sorted

    half :: String -> B.Token -> [B.TokenTree] -> [B.Named [B.TokenTree]] -> C.HalfRelmap
    half usage tok opd sorted =
        C.HalfRelmap usage tok (("operand", opd) : sorted) []

    subrelmap :: B.AbMap C.HalfRelmap
    subrelmap h@C.HalfRelmap { C.halfOperand = opd } =
        case lookup "-relmap" opd of
          Nothing    -> Right h   -- no subrelmaps
          Just trees -> do subHs <- mapM (consHalf . B.singleton) trees
                           Right $ h { C.halfSubrelmap = subHs }


-- ----------------------  Full construction

{-| Second step of constructing relmap,
    make 'C.Relmap' from contents of 'C.HalfRelmap'. -}
type RelmapConsFull c
    = C.HalfRelmap        -- ^ Half relmap
    -> B.Ab (C.Relmap c)  -- ^ Result full relmap

{-| Construct (full) relmap. -}
makeConsFull :: [B.Named (C.RopCons c)] -> RelmapConsFull c
makeConsFull fulls = consFull where
    consFull half =
        let op    = C.halfOpText    half
            subHs = C.halfSubrelmap half
        in case lookup op fulls of
             Nothing   -> Right $ C.RelmapName half op
             Just cons -> do subFs <- mapM consFull subHs
                             cons $ C.RopUse half subFs


-- ----------------------
{- $ConstructionProcess
  
   Construction process of half relmaps from source trees.
  
   [@\[TokenTree\] -> \[\[TokenTree\]\]@]
      Dicide list of 'B.TokenTree' by vertical bar (@|@).
  
   [@\[\[TokenTree\]\] -> \[HalfRelmap\]@]
      Construct each 'C.HalfRelmap' from lists of 'B.TokenTree'.
      When there are subrelmaps in token trees,
      constructs 'C.HalfRelmap' recursively.
  
   [@\[HalfRelmap\] -> HalfRelmap@]
      Wrap list of 'C.HalfRelmap' into one 'C.HalfRelmap'
      that has these relmaps in 'C.halfSubrelmap'.
-}

