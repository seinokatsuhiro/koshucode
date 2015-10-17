{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Gadget
  ( ropsGadget,
  
    -- * contents
    consContents, relmapContents,
  
    -- * dependent-rank
    consDepRank, relmapDepRank,
  
    -- * visit-distance
    consVisitDistance, relmapVisitDistance,
  
    -- * size
    consSize, relmapSize, relkitSize,
    -- $size
  
    -- * eqlize
    consEqlize, relmapEqlize, relkitEqlize,

    -- * dump-tree
    consDumpTree,
  ) where

import qualified Data.Map.Strict            as Map
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as D
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.DepRank as Op
import qualified Koshucode.Baala.Op.Message as Msg


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
ropsGadget :: (D.CContent c) => [C.Rop c]
ropsGadget = Op.ropList "gadget"  -- GROUP
    --        CONSTRUCTOR       USAGE                      ATTRIBUTE
    [ Op.def  consContents      "contents /N"              "V -term"
    , Op.def  consDumpTree      "dump-tree X"              "V -tree"
    , Op.def  consDepRank       "dependent-rank /P /P -rank /N"     "2 -x -y | -rank"
    , Op.def  consEqlize        "eqlize"                   "0"
    , Op.def  consVisitDistance "visit-distance R -step /P ... -to /N -distance /N"  "1 -relmap/ | -step -to -distance"
    , Op.def  consSize          "size /N"                  "1 -term"
    ]


-- ----------------------  contents

consContents :: (Ord c) => C.RopCons c
consContents med =
    do n <- Op.getTerm med "-term"
       Right $ relmapContents med n

relmapContents :: (Ord c) => C.Intmed c -> D.TermName -> C.Relmap c
relmapContents med = C.relmapFlow med . relkitContents

relkitContents :: (Ord c) => D.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitJust he2 $ C.RelkitFull False kitf where
    he2  = D.headFrom [n]
    kitf = map B.li1 . B.unique . concat


-- ----------------------  dependent-rank

--  dependent-rank /x /y -rank /r

consDepRank :: (Ord c, D.CDec c) => C.RopCons c
consDepRank med =
    do x <- Op.getTerm med "-x"
       y <- Op.getTerm med "-y"
       r <- Op.getTerm med "-rank"
       Right $ relmapDepRank med (x,y,r)

relmapDepRank :: (Ord c, D.CDec c) => C.Intmed c -> D.TermName3 -> C.Relmap c
relmapDepRank med = C.relmapFlow med . relkitDepRank

relkitDepRank :: (Ord c, D.CDec c) => D.TermName3 -> C.RelkitFlow c
relkitDepRank _  Nothing = Right C.relkitNothing
relkitDepRank (x,y,r) (Just he1) = Right kit2 where
    he2         = D.headFrom [x,r]
    kit2        = C.relkitJust he2 $ C.RelkitFull False f2
    xyPick      = Op.picker he1 [x,y]
    f2 bo1      = map put $ rank $ map get bo1
    get cs      = let [cx,cy] = xyPick cs in (cx,cy)
    put (cx,i)  = [cx, D.pInt i]
    rank        = Op.depRankList . Op.depRankUpdateAll . Op.depRankFromPairs


-- ----------------------  visit-distance

--  visit-distance r -step /a /b : /c /d -to /v -distance /n

consVisitDistance :: (Ord c, D.CDec c, D.CRel c) => C.RopCons c
consVisitDistance med =
    do rmap  <- Op.getRelmap med "-relmap"
       to    <- Op.getTerm   med "-to"
       dist  <- Op.getTerm   med "-distance"
       steps <- Op.getTermsColon med "-step"
       case steps of
         [step1, step2] -> Right $ relmapVisitDistance med (step1, step2, to, dist) rmap
         _              -> Msg.adlib "Require two sets of terms"

relmapVisitDistance :: (Ord c, D.CDec c, D.CRel c) => C.Intmed c -> ([D.TermName], [D.TermName], D.TermName, D.TermName) -> B.Map (C.Relmap c)
relmapVisitDistance med = C.relmapBinary med . relkitVisitDistance

relkitVisitDistance :: (Ord c, D.CDec c, D.CRel c) => ([D.TermName], [D.TermName], D.TermName, D.TermName) -> C.RelkitBinary c
relkitVisitDistance (step1, step2, to, dist) (C.Relkit _ (Just he2) kitb2) (Just he1)
    | unkStart /= []     = Msg.unkTerm unkStart he1
    | unkFrom  /= []     = Msg.unkTerm unkFrom  he2
    | unkTo    /= []     = Msg.unkTerm unkTo    he2
    | dup      /= []     = Msg.dupAttr dup
    | newDist  == []     = Msg.adlib "Require new term"
    | lenFrom  /= lenTo  = Msg.adlib "Require same number of terms"
    | otherwise          = Right kit3
    where
      lrStart   = step1 `D.headLROrd` D.headNames he1
      lrDist    = [to]  `D.headLROrd` D.headNames he1
      lrFrom    = step1 `D.headLROrd` D.headNames he2
      lrTo      = step2 `D.headLROrd` D.headNames he2

      unkStart  = D.headLSideNames lrStart
      unkFrom   = D.headLSideNames lrFrom
      unkTo     = D.headLSideNames lrTo
      newDist   = D.headLSideNames lrDist

      lenFrom   = length step1
      lenTo     = length step2
      dup       = B.duplicates $ dist : step1
      he3       = D.headConsNest to heTo he1
      heTo      = D.headFrom $ dist : step1
      kit3      = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
      kitf3     = relkitVisitDistanceBody (lenFrom == 1) lrStart lrFrom lrTo heTo

relkitVisitDistance _ _ _ = Right C.relkitNothing

relkitVisitDistanceBody :: (D.CDec c, D.CRel c, Ord c) =>
  Bool -> D.HeadLR c -> D.HeadLR c -> D.HeadLR c -> D.Head -> [C.BodyMap c] -> B.AbMap [[c]]
relkitVisitDistanceBody optimize1 lrStart lrFrom lrTo heTo
    | optimize1 = kitf3 (add1, tuple1, vdist1)
    | otherwise = kitf3 (addN, tupleN, vdistN)
    where
      kitf3 x@(addX, _, _) bmaps bo1 =
          do [bo2] <- calcBody bmaps bo1
             let vstep = foldr addX Map.empty bo2
             Right $ calc x vstep `map` bo1

      calc (_, tupleX, vdistX) vstep cs1 = rel : cs1 where
          rel    = D.pRel $ D.Rel heTo body
          body   = map tupleX $ Map.assocs $ vdistX vstep start
          start  = D.headRShare lrStart cs1

      vdistN vstep cs   = visitDistanceFrom vstep cs
      vdist1 vstep [c]  = visitDistanceFrom vstep c
      vdist1 _ _        = B.bug "visit-distance"

      tupleN (cs, n)    = D.pInt n : cs
      tuple1 (c, n)     = [D.pInt n, c]

      addN cs           = insertPush (D.headRShare lrFrom cs)
                                     (D.headRShare lrTo cs)
      add1 cs           = insertPush (head $ D.headRShare lrFrom cs)
                                     (head $ D.headRShare lrTo cs)

calcBody :: [C.BodyMap c] -> [[c]] -> B.Ab [[[c]]]
calcBody bmaps bo = (\bmap -> bmap bo) `mapM` bmaps

-- | Mapping node to next nodes.
type VisitStep a = Map.Map a [a]

-- | Mapping node to its distance.
type VisitDist a = Map.Map a Int

visitDistanceFrom :: (Ord a) => VisitStep a -> a -> VisitDist a
visitDistanceFrom step a = visitDistance step $ Map.fromList [(a, 0)]

visitDistance :: (Ord a) => VisitStep a -> VisitDist a -> VisitDist a
visitDistance step = visit 1 where
    sameSize x y = Map.size x == Map.size y
    visit n dist = case visitDistanceStep step dist n of
                     dist' | sameSize dist dist' -> dist'
                           | otherwise           -> visit (n + 1) dist'

visitDistanceStep :: (Ord a) => VisitStep a -> VisitDist a -> Int -> VisitDist a
visitDistanceStep step dist n = foldr insert dist toList where
    insert a  = insertFirst a n
    toList    = next $ Map.keys dist
    next      = concat . B.mapMaybe (`Map.lookup` step)

insertFirst :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
insertFirst = Map.insertWith first where
    first _ old = old

insertPush :: (Ord k) => k -> a -> B.Map (Map.Map k [a])
insertPush k a = Map.insertWith push k [a] where
    push _ as = a : as


-- ----------------------  size

-- $size
--
--  Count number of tuples in the output of relmap @a@.
--  
--    > a | size /c
--

consSize :: (D.CDec c) => C.RopCons c
consSize med =
  do n <- Op.getTerm med "-term"
     Right $ relmapSize med n

relmapSize :: (D.CDec c) => C.Intmed c -> D.TermName -> C.Relmap c
relmapSize med n = C.relmapFlow med $ relkitSize n

relkitSize :: (D.CDec c) => D.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = D.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ D.pInt $ length bo1 ]]


-- ----------------------  eqlize

consEqlize :: (Ord c) => C.RopCons c
consEqlize med = Right $ relmapEqlize med

relmapEqlize :: (Ord c) => C.Intmed c -> C.Relmap c
relmapEqlize med = C.relmapFlow med relkitEqlize

relkitEqlize :: (Ord c) => C.RelkitFlow c
relkitEqlize Nothing = Right C.relkitNothing
relkitEqlize (Just he1) = Right kit2 where
    kit2       = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 bo1  = case eqlizeBody Map.empty bo1 of
                   (_, bo2) -> bo2

eqlize :: (Ord c) => Map.Map c c -> c -> (Map.Map c c, c)
eqlize m c = case Map.lookup c m of
               Just c' -> (m, c')
               Nothing -> (Map.insert c c m, c)

eqlizeList :: (Ord c) => Map.Map c c -> [c] -> (Map.Map c c, [c])
eqlizeList = loop where
    loop m [] = (m, [])
    loop m (c : cs) = let (m1, c')  = eqlize m c
                          (m2, cs') = loop m1 cs
                      in (m2, c' : cs')

eqlizeBody :: (Ord c) => Map.Map c c -> [[c]] -> (Map.Map c c, [[c]])
eqlizeBody = loop where
    loop m [] = (m, [])
    loop m (t : ts) = let (m1, t')  = eqlizeList m t
                          (m2, ts') = loop m1 ts
                      in (m2, t' : ts')


-- ----------------------  dump-tree

consDumpTree :: (D.CDec c) => C.RopCons c
consDumpTree med =
  do trees <- Op.getTrees med "-tree"
     Msg.dumpTrees trees

