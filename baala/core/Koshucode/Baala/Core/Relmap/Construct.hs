{-# OPTIONS_GHC -Wall #-}

-- | 'C.Relmap' construction.

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
import qualified Koshucode.Baala.Core.Relmap.Rop        as C


-- ----------------------  Constructions

-- | Make half and full relmap constructors.
relmapCons :: C.Global c -> (RelmapCons c)
relmapCons global = make $ unzip $ map pair $ C.globalRops global where
    make (halfs, fulls) =
        let consHalf = relmapConsHalf halfs
            consFull = relmapConsFull global fulls
        in RelmapCons consHalf consFull

    pair (C.Rop name _ sorter cons synopsis) =
        let half = (name, (synopsis, sorter))
            full = (name, cons)
        in (half, full)

-- | Half and full relmap constructor
data RelmapCons c = RelmapCons RelmapConsHalf (RelmapConsFull c)

instance Show (RelmapCons c) where
    show _ = "RelmapCons <half> <full>"


-- ----------------------  Half construction

-- | First step of constructing relmap,
--   make 'C.HalfRelmap' from use of relmap operator.
type RelmapConsHalf
    =  [B.TokenTree]      -- ^ Source of relmap operator
    -> B.Ab C.HalfRelmap  -- ^ Result half relmap

relmapConsHalf :: [B.Named (String, C.RopFullSorter)] -> RelmapConsHalf
relmapConsHalf halfs = consHalf where
    consHalf :: RelmapConsHalf
    consHalf trees =
        B.abortable "half" (B.front $ B.untrees trees) $
         case B.divideTreesByBar trees of
           [(B.TreeL tok@(B.TWord _ 0 _) : od)] -> find tok od
           [[B.TreeB 1 _ xs]] -> consHalf xs
           [[B.TreeB _ _ _]]  -> Left $ B.AbortAnalysis [] $ B.AAUndefined "bracket"
           [_]                -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap "?"
           tree2              -> find (B.tokenWord "append") $ map B.treeWrap tree2

    find :: B.Token -> [B.TokenTree] -> B.Ab C.HalfRelmap
    find op trees =
        case lookup (B.tokenContent op) halfs of
          Nothing -> Right $ half op trees [] "<reference>"
          Just (usage, operandSorter) ->
              do sorted <- operandSorter trees
                 subrelmap $ half op trees sorted usage

    half :: B.Token -> [B.TokenTree] -> [B.Named [B.TokenTree]] -> String -> C.HalfRelmap
    half op trees sorted usage =
        C.HalfRelmap op (("operand", trees) : sorted) [] usage

    subrelmap :: B.AbMap C.HalfRelmap
    subrelmap h@C.HalfRelmap { C.halfOperand = od } =
        case lookup "-relmap" od of
          Nothing    -> Right h   -- no subrelmaps
          Just trees -> do subs <- mapM (consHalf . B.singleton) trees
                           Right $ h { C.halfSubrelmap = subs }


-- ----------------------  Full construction

-- | Second step of constructing relmap,
--   make 'C.Relmap' from contents of 'C.HalfRelmap'.
type RelmapConsFull c
    = C.HalfRelmap        -- ^ Half relmap
    -> B.Ab (C.Relmap c)  -- ^ Result full relmap

-- | Construct (full) relmap.
relmapConsFull :: C.Global c -> [B.Named (C.RopCons c)] -> RelmapConsFull c
relmapConsFull global fulls = consFull where
    consFull half =
        let op    = C.halfOpText    half
            subHs = C.halfSubrelmap half
        in case lookup op fulls of
             Nothing   -> Right $ C.RelmapLink half op Nothing
             Just cons -> B.abortableFrom "relmap" half $
                          do subFs <- mapM consFull subHs
                             cons $ C.RopUse global half subFs


-- ----------------------
-- $ConstructionProcess
--
--  Construction process of half relmaps from source trees.
--
--  [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--     Dicide list of 'B.TokenTree' by vertical bar (@|@).
--
--  [@\[\[TokenTree\]\] -> \[HalfRelmap\]@]
--     Construct each 'C.HalfRelmap' from lists of 'B.TokenTree'.
--     When there are subrelmaps in token trees,
--     constructs 'C.HalfRelmap' recursively.
--
--  [@\[HalfRelmap\] -> HalfRelmap@]
--     Wrap list of 'C.HalfRelmap' into one 'C.HalfRelmap'
--     that has these relmaps in 'C.halfSubrelmap'.
--
