{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Gadget
( ropsGadget,

  -- * contents
  consContents, relmapContents,

  -- * size
  consSize, relmapSize, relkitSize,
  -- $size

  -- * number
  consNumber, relmapNumber, relkitNumber,

  -- * rank
  consRank,
  relmapGapRank, relkitGapRank,
  relmapDenseRank, relkitDenseRank,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Op.Builtin as Op


-- | Gadgets
--
--   [@contents@]
--     Make nary relation of all contetnts.
--
--   [@number \/N \[ -order \/P ... \]@]
--     Add numbering term @\/N@ ordered by @\/P@ ...
-- 
--   [@rank \/N -order \/P ... \[ -dense \]@]
--     Add term @\/N@ for ranking ordered by @\/P@ ...
-- 
--   [@size \/N@]
--     Calculate cardinality of input relation.
--
ropsGadget :: (C.CContent c) => [C.Rop c]
ropsGadget = Op.ropList "gadget"  -- GROUP
    --   USAGE                    , CONSTRUCTOR    , ATTRIBUTE
    [ ( "contents /N"             , consContents   , C.roaList "-term" [] )
    , ( "number /N -order /N ..." , consNumber     , C.roaOne "-term" ["-order", "-from"] )
    , ( "rank /N -order /N ..."   , consRank       , C.roaOne "-term" ["-order", "-dense"] )
    , ( "size /N"                 , consSize       , C.roaOne "-term" [] )
    ]


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents use =
    do n <- Op.getTerm use "-term"
       Right $ relmapContents use n

relmapContents :: (Ord c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapContents use = C.relmapFlow use . relkitContents

relkitContents :: (Ord c) => B.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = B.headFrom [n]
    kitf = map B.li1 . B.unique . concat


-- ----------------------  size

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c
--

consSize :: (C.CDec c) => C.RopCons c
consSize use =
  do n <- Op.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapSize use n = C.relmapFlow use $ relkitSize n

relkitSize :: (C.CDec c) => B.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = B.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pDecFromInt $ length bo1 ]]


-- ----------------------  number

consNumber :: (Ord c, C.CDec c) => C.RopCons c
consNumber use =
    do n    <- Op.getTerm use "-term"
       ns   <- Op.getOption [] Op.getTerms use "-order"
       from <- Op.getOption 0  Op.getInt   use "-from"
       Right $ relmapNumber use (n, ns, from)

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapNumber use = C.relmapFlow use . relkitNumber

relkitNumber :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.TermName c
    -> (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitRanking _ _ Nothing = Right C.relkitNothing
relkitRanking ranking (n, ns, from) (Just he1) = Right kit2 where
    he2   = B.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = let (rank, bo2) = ranking from ords (B.headNames he1) bo1
                in zipWith (:) (map C.pDecFromInt rank) bo2
    ords  = map B.Asc ns


-- ----------------------  rank

consRank :: (Ord c, C.CDec c) => C.RopCons c
consRank use =
    do n     <- Op.getTerm   use "-term"
       ns    <- Op.getTerms  use "-order"
       dense <- Op.getSwitch use "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank use (n, ns, 0)

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapDenseRank use = C.relmapFlow use . relkitDenseRank

relkitDenseRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapGapRank use = C.relmapFlow use . relkitGapRank

relkitGapRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking B.sortByNameGapRank

