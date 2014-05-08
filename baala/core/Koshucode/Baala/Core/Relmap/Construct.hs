{-# OPTIONS_GHC -Wall #-}

-- | Construct 'C.Lexmap' and 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
( -- * Data type
  RelmapCons (..),
  ConsLexmap,
  ConsRelmap,

  -- * Function
  relmapCons,

  -- * Construction process
  -- $ConstructionProcess
) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Relmap.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap.Operand   as C
import qualified Koshucode.Baala.Core.Relmap.Operator  as C
import qualified Koshucode.Baala.Core.Message          as Message


-- ----------------------  Constructions

-- | Constructor of lexmap and relmap.
data RelmapCons c = RelmapCons
      { consLexmap :: ConsLexmap
      , consRelmap :: ConsRelmap c
      }

instance Show (RelmapCons c) where
    show _ = "RelmapCons <lex> <full>"

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: C.Global c -> (RelmapCons c)
relmapCons g = make $ unzip $ map pair $ C.globalRops g where
    make (l, r) = RelmapCons (makeConsLexmap l) (makeConsRelmap g r)
    pair (C.Rop n _ sorter cons _) = ((n, sorter), (n, cons))



-- ----------------------  Lexmap

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [B.TokenTree] -> B.Ab C.Lexmap

makeConsLexmap :: [B.Named C.RodSorter] -> ConsLexmap
makeConsLexmap lxs = consLex where
    consLex :: ConsLexmap
    consLex trees =
        B.abortable "lexmap" (B.front $ B.untrees trees) $
         case B.divideTreesByBar trees of
           [(B.TreeL tok@(B.TWord _ 0 _) : od)] -> find tok od
           [[B.TreeB 1 _ xs]] -> consLex xs
           [[B.TreeB _ _ _]]  -> Message.adlib "bracket"
           [_]                -> Message.unkRelmap "?"
           tree2              -> find (B.tokenWord "append") $ map B.treeWrap tree2

    find :: B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    find op trees =
        case lookup (B.tokenContent op) lxs of
          Nothing ->
              do sorted <- C.rodBranch trees
                 Right $ lexmap op trees sorted
          Just operandSorter ->
              do sorted <- operandSorter trees
                 submap $ lexmap op trees sorted

    lexmap :: B.Token -> [B.TokenTree] -> [B.NamedTrees] -> C.Lexmap
    lexmap op trees sorted =
        C.Lexmap op (("@operand", trees) : sorted) []

    submap :: B.AbMap C.Lexmap
    submap lx@C.Lexmap { C.lexOperand = od } =
        case lookup "-relmap" $ B.mapFstTo (take 7) od of
          Nothing    -> Right lx   -- no submaps
          Just trees -> do subs <- mapM (consLex . B.singleton) trees
                           Right $ lx { C.lexSubmap = subs }



-- ----------------------  Generic relmap

-- | Second step of constructing relmap,
--   make relmap from contents of lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

makeConsRelmap :: C.Global c -> [B.Named (C.RopCons c)] -> ConsRelmap c
makeConsRelmap global fulls = relmap where
    relmap lx =
        let rop  = C.lexOpText  lx
            rod  = C.lexOperand lx
            lxs  = C.lexSubmap  lx
        in case lookup rop fulls of
             Nothing   -> Right $ C.RelmapLink lx rop rod
             Just cons -> B.abortableFrom "relmap" lx $
                          do rmaps <- mapM relmap lxs
                             cons $ C.RopUse global lx rmaps


-- ----------------------
-- $ConstructionProcess
--
--  Construction process of lex relmaps from source trees.
--
--  [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--     Divide list of 'B.TokenTree' by vertical bar (@|@).
--
--  [@\[\[TokenTree\]\] -> \[Lexmap\]@]
--     Construct each 'C.Lexmap' from lists of 'B.TokenTree'.
--     When there are submaps in token trees,
--     constructs 'C.Lexmap' recursively.
--
--  [@\[Lexmap\] -> Lexmap@]
--     Wrap list of 'C.Lexmap' into one 'C.Lexmap'
--     that has these relmaps in 'C.lexSubmap'.
--
