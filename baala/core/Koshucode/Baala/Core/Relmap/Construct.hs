{-# OPTIONS_GHC -Wall #-}

-- | 'C.Relmap' construction.

module Koshucode.Baala.Core.Relmap.Construct
( relmapCons,
  RelmapCons (..),
  RelmapConsLex,
  RelmapConsFull,

  -- * Construction process
  -- $ConstructionProcess
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Relmap.Lexical    as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Rop        as C


-- ----------------------  Constructions

-- | Make lex and full relmap constructors.
relmapCons :: C.Global c -> (RelmapCons c)
relmapCons global = make $ unzip $ map pair $ C.globalRops global where
    make (lxs, fulls) =
        let consLex  = relmapConsLex lxs
            consFull = relmapConsFull global fulls
        in RelmapCons consLex consFull

    pair (C.Rop name _ sorter cons synopsis) =
        let lx = (name, (synopsis, sorter))
            full = (name, cons)
        in (lx, full)

-- | Lex and full relmap constructor
data RelmapCons c = RelmapCons RelmapConsLex (RelmapConsFull c)

instance Show (RelmapCons c) where
    show _ = "RelmapCons <lex> <full>"


-- ----------------------  Lex construction

-- | First step of constructing relmap,
--   make 'C.Lexmap' from use of relmap operator.
type RelmapConsLex
    =  [B.TokenTree]   -- ^ Source of relmap operator
    -> B.Ab C.Lexmap   -- ^ Result lex relmap

relmapConsLex :: [B.Named (String, C.RopFullSorter)] -> RelmapConsLex
relmapConsLex lxs = consLex where
    consLex :: RelmapConsLex
    consLex trees =
        B.abortable "lex" (B.front $ B.untrees trees) $
         case B.divideTreesByBar trees of
           [(B.TreeL tok@(B.TWord _ 0 _) : od)] -> find tok od
           [[B.TreeB 1 _ xs]] -> consLex xs
           [[B.TreeB _ _ _]]  -> Left $ B.AbortAnalysis [] $ B.AAUndefined "bracket"
           [_]                -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap "?"
           tree2              -> find (B.tokenWord "append") $ map B.treeWrap tree2

    find :: B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    find op trees =
        case lookup (B.tokenContent op) lxs of
          Nothing -> Right $ lx op trees [] "<reference>"
          Just (usage, operandSorter) ->
              do sorted <- operandSorter trees
                 subrelmap $ lx op trees sorted usage

    lx :: B.Token -> [B.TokenTree] -> [B.Named [B.TokenTree]] -> String -> C.Lexmap
    lx op trees sorted usage =
        C.Lexmap op (("operand", trees) : sorted) [] usage

    subrelmap :: B.AbMap C.Lexmap
    subrelmap h@C.Lexmap { C.lexOperand = od } =
        case lookup "-relmap" od of
          Nothing    -> Right h   -- no subrelmaps
          Just trees -> do subs <- mapM (consLex . B.singleton) trees
                           Right $ h { C.lexSubrelmap = subs }


-- ----------------------  Full construction

-- | Second step of constructing relmap,
--   make 'C.Relmap' from contents of 'C.Lexmap'.
type RelmapConsFull c
    = C.Lexmap            -- ^ Lexical relmap
    -> B.Ab (C.Relmap c)  -- ^ Result full relmap

-- | Construct (full) relmap.
relmapConsFull :: C.Global c -> [B.Named (C.RopCons c)] -> RelmapConsFull c
relmapConsFull global fulls = consFull where
    consFull lx =
        let op    = C.lexOpText lx
            subHs = C.lexSubrelmap lx
        in case lookup op fulls of
             Nothing   -> Right $ C.RelmapLink lx op Nothing
             Just cons -> B.abortableFrom "relmap" lx $
                          do subFs <- mapM consFull subHs
                             cons $ C.RopUse global lx subFs


-- ----------------------
-- $ConstructionProcess
--
--  Construction process of lex relmaps from source trees.
--
--  [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--     Dicide list of 'B.TokenTree' by vertical bar (@|@).
--
--  [@\[\[TokenTree\]\] -> \[Lexmap\]@]
--     Construct each 'C.Lexmap' from lists of 'B.TokenTree'.
--     When there are subrelmaps in token trees,
--     constructs 'C.Lexmap' recursively.
--
--  [@\[Lexmap\] -> Lexmap@]
--     Wrap list of 'C.Lexmap' into one 'C.Lexmap'
--     that has these relmaps in 'C.lexSubrelmap'.
--
