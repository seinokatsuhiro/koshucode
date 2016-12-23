{-# OPTIONS_GHC -Wall #-}

-- | Gadgets.

module Koshucode.Baala.Rop.Flat.Gadget
  ( ropsGadget,
    -- * contents
    consContents, relmapContents,
    -- * partial-order
    consPoHeight,
    consPoDepth,
    relmapPoScale, relkitPoScale,
    -- * visit-distance
    consVisitDistance, relmapVisitDistance,
    -- * size
    consSize, relmapSize, relkitSize,
    -- * eqlize
    consEqlize, relmapEqlize, relkitEqlize,
    -- * dump-tree
    consDumpTree,
  ) where

import qualified Data.Map.Strict                   as Ms
import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat.PoScale  as Rop
import qualified Koshucode.Baala.Rop.Flat.Subtext  as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Gadgets
ropsGadget :: (K.CContent c) => [C.Rop c]
ropsGadget = Rop.rops "gadget"
    [ consContents       K.& [ "contents /N"
                               K.& "-term*" ]
    , consDumpTree       K.& [ "dump-tree X"
                               K.& "-tree*" ]
    , consEqlize         K.& [ "eqlize"
                               K.& "" ]
    , consPoDepth        K.& [ "partial-order-depth /P /P -to /N /N"
                               K.& "-x -y . -to" ]
    , consPoHeight       K.& [ "partial-order-height /P /P -to /N /N"
                               K.& "-x -y . -to" ]
    , consVisitDistance  K.& [ "visit-distance R -step /P ... -to /N -distance /N"
                               K.& "-relmap/ . -step -to -distance" ]
    , consSize           K.& [ "size /N"
                               K.& "-term" ]
    , Rop.consSubtext    K.& [ "subtext /N E"
                               K.& "-term -subtext . -trim?" ]
    ]


-- ----------------------  contents

-- | __contents \/N__
--
--   Collect all contents in unary relation.
--
consContents :: (Ord c) => C.RopCons c
consContents med =
    do n <- Rop.getTerm med "-term"
       Right $ relmapContents med n

-- | Create @contents@ relmap.
relmapContents :: (Ord c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapContents med = C.relmapFlow med . relkitContents

-- | Create @contents@ relkit.
relkitContents :: (Ord c) => K.TermName -> C.RelkitFlow c
relkitContents n _ = Right $ C.relkitWhole False he2 flow where
    he2  = K.headFrom [n]
    flow = map K.list1 . K.unique . concat


-- ----------------------  partial-order

--  partial-order-height /x /y -to /z /ht
--  partial-order-depth /x /y -to /z /dp

-- | __partial-order-height \/P \/P -to \/N \/N__
consPoHeight :: (Ord c, K.CDec c) => C.RopCons c
consPoHeight = consPoScale Rop.poScaleHeight

-- | __partial-order-depth \/P \/P -to \/N \/N__
consPoDepth :: (Ord c, K.CDec c) => C.RopCons c
consPoDepth = consPoScale Rop.poScaleDepth

consPoScale :: (Ord c, K.CDec c) => Rop.PoScaleCalc c -> C.RopCons c
consPoScale scale med =
    do x <- Rop.getTerm med "-x"
       y <- Rop.getTerm med "-y"
       (z,r) <- Rop.getTerm2 med "-to"
       Right $ relmapPoScale scale med (x,y,z,r)

-- | Create @partial-order-height@ or @partial-order-depth@ relmap.
relmapPoScale :: (Ord c, K.CDec c) => Rop.PoScaleCalc c -> C.Intmed c -> K.TermName4 -> C.Relmap c
relmapPoScale scale med = C.relmapFlow med . relkitPoScale scale

-- | Create @partial-order-height@ or @partial-order-depth@ relkit.
relkitPoScale :: (Ord c, K.CDec c) => Rop.PoScaleCalc c -> K.TermName4 -> C.RelkitFlow c
relkitPoScale _ _ Nothing = C.relkitUnfixed
relkitPoScale scale (x,y,z,r) (Just he1) = Right kit2 where
    he2         = K.headFrom [z,r]
    kit2        = C.relkitWhole False he2 flow
    xyPick      = K.pickDirect [x,y] he1
    flow bo1    = map put $ scale $ map get bo1
    get cs      = let [cx,cy] = xyPick cs in (cx,cy)
    put (cx,i)  = [cx, K.pInt i]


-- ----------------------  visit-distance

--  visit-distance r -step /a /b : /c /d -to /v -distance /n

-- | __visit-distance R -step \/P ... -to \/N -distance \/N__
consVisitDistance :: (Ord c, K.CDec c, K.CRel c) => C.RopCons c
consVisitDistance med =
    do rmap  <- Rop.getRelmap med "-relmap"
       to    <- Rop.getTerm   med "-to"
       dist  <- Rop.getTerm   med "-distance"
       steps <- Rop.getTermsColon med "-step"
       case steps of
         [step1, step2] -> Right $ relmapVisitDistance med (step1, step2, to, dist) rmap
         _              -> Msg.adlib "Require two sets of terms"

-- | Create @visit-distance@ relmap.
relmapVisitDistance :: (Ord c, K.CDec c, K.CRel c) => C.Intmed c -> ([K.TermName], [K.TermName], K.TermName, K.TermName) -> K.Map (C.Relmap c)
relmapVisitDistance med = C.relmapBinary med . relkitVisitDistance

-- | Create @visit-distance@ relkit.
relkitVisitDistance :: (Ord c, K.CDec c, K.CRel c) => ([K.TermName], [K.TermName], K.TermName, K.TermName) -> C.RelkitBinary c
relkitVisitDistance (step1, step2, to, dist) (C.RelkitOutput he2 kitb2) (Just he1)
    | K.newTermsExist pkStart   = Msg.newTerm pkStart he1
    | K.newTermsExist pkFrom    = Msg.newTerm pkFrom  he2
    | K.newTermsExist pkTo      = Msg.newTerm pkTo    he2
    | dup      /= []     = Msg.dupAttr dup
    | newDist  == []     = Msg.adlib "Require new term"
    | lenFrom  /= lenTo  = Msg.adlib "Require same number of terms"
    | otherwise          = Right kit3
    where
      pkStart   = K.termPicker step1 he1
      pkDist    = K.termPicker [to]  he1
      pkFrom    = K.termPicker step1 he2
      pkTo      = K.termPicker step2 he2

      newDist   = K.pkLProperNames pkDist

      lenFrom   = length step1
      lenTo     = length step2
      dup       = K.duplicates $ dist : step1
      he3       = K.headConsNest to heTo he1
      heTo      = K.headFrom $ dist : step1
      kit3      = C.relkitConflWhole False he3 confl [kitb2]
      confl     = relkitVisitDistanceBody (lenFrom == 1) pkStart pkFrom pkTo heTo

relkitVisitDistance _ _ _ = C.relkitUnfixed

relkitVisitDistanceBody :: (K.CDec c, K.CRel c, Ord c) =>
  Bool -> K.TermPicker c -> K.TermPicker c -> K.TermPicker c -> K.Head -> [C.BodyMap c] -> K.AbMap [[c]]
relkitVisitDistanceBody optimize1 pkStart pkFrom pkTo heTo
    | optimize1 = kitf3 (add1, tuple1, vdist1)
    | otherwise = kitf3 (addN, tupleN, vdistN)
    where
      kitf3 x@(addX, _, _) bmaps bo1 =
          do [bo2] <- calcBody bmaps bo1
             let vstep = foldr addX Ms.empty bo2
             Right $ calc x vstep `map` bo1

      calc (_, tupleX, vdistX) vstep cs1 = rel : cs1 where
          rel    = K.pRel $ K.Rel heTo body
          body   = map tupleX $ Ms.assocs $ vdistX vstep start
          start  = K.pkRShare pkStart cs1

      vdistN vstep cs   = visitDistanceFrom vstep cs
      vdist1 vstep [c]  = visitDistanceFrom vstep c
      vdist1 _ _        = K.bug "visit-distance"

      tupleN (cs, n)    = K.pInt n : cs
      tuple1 (c, n)     = [K.pInt n, c]

      addN cs           = insertPush (K.pkRShare pkFrom cs)
                                     (K.pkRShare pkTo cs)
      add1 cs           = insertPush (head $ K.pkRShare pkFrom cs)
                                     (head $ K.pkRShare pkTo cs)

calcBody :: [C.BodyMap c] -> [[c]] -> K.Ab [[[c]]]
calcBody bmaps bo = (\bmap -> bmap bo) `mapM` bmaps

-- | Mapping node to next nodes.
type VisitStep a = Ms.Map a [a]

-- | Mapping node to its distance.
type VisitDist a = Ms.Map a Int

visitDistanceFrom :: (Ord a) => VisitStep a -> a -> VisitDist a
visitDistanceFrom step a = visitDistance step $ Ms.fromList [(a, 0)]

visitDistance :: (Ord a) => VisitStep a -> VisitDist a -> VisitDist a
visitDistance step = visit 1 where
    sameSize x y = Ms.size x == Ms.size y
    visit n dist = case visitDistanceStep step dist n of
                     dist' | sameSize dist dist' -> dist'
                           | otherwise           -> visit (n + 1) dist'

visitDistanceStep :: (Ord a) => VisitStep a -> VisitDist a -> Int -> VisitDist a
visitDistanceStep step dist n = foldr insert dist toList where
    insert a  = insertFirst a n
    toList    = next $ Ms.keys dist
    next      = concat . K.mapMaybe (`Ms.lookup` step)

insertFirst :: (Ord k) => k -> a -> Ms.Map k a -> Ms.Map k a
insertFirst = Ms.insertWith first where
    first _ old = old

insertPush :: (Ord k) => k -> a -> K.Map (Ms.Map k [a])
insertPush k a = Ms.insertWith push k [a] where
    push _ as = a : as


-- ----------------------  size

-- | __size \/N__
--
--  Count number of tuples.
--
consSize :: (K.CDec c) => C.RopCons c
consSize med =
  do n <- Rop.getTerm med "-term"
     Right $ relmapSize med n

-- | Create @size@ relmap.
relmapSize :: (K.CDec c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapSize med n = C.relmapFlow med $ relkitSize n

-- | Create @size@ relkit.
relkitSize :: (K.CDec c) => K.TermName -> C.RelkitFlow c
relkitSize n _ = Right kit2 where
    he2       = K.headFrom [n]
    kit2      = C.relkitWhole False he2 flow
    flow bo1  = [[ K.pInt $ length bo1 ]]


-- ----------------------  eqlize

-- | __eqlize__
consEqlize :: (Ord c) => C.RopCons c
consEqlize med = Right $ relmapEqlize med

-- | Create @eqlize@ relmap.
relmapEqlize :: (Ord c) => C.Intmed c -> C.Relmap c
relmapEqlize med = C.relmapFlow med relkitEqlize

-- | Create @eqlize@ relkit.
relkitEqlize :: (Ord c) => C.RelkitFlow c
relkitEqlize Nothing = C.relkitUnfixed
relkitEqlize (Just he1) = Right kit2 where
    kit2      = C.relkitWhole False he1 flow
    flow bo1  = case eqlizeBody Ms.empty bo1 of
                  (_, bo2) -> bo2

eqlize :: (Ord c) => Ms.Map c c -> c -> (Ms.Map c c, c)
eqlize m c = case Ms.lookup c m of
               Just c' -> (m, c')
               Nothing -> (Ms.insert c c m, c)

eqlizeList :: (Ord c) => Ms.Map c c -> [c] -> (Ms.Map c c, [c])
eqlizeList = loop where
    loop m [] = (m, [])
    loop m (c : cs) = let (m1, c')  = eqlize m c
                          (m2, cs') = loop m1 cs
                      in (m2, c' : cs')

eqlizeBody :: (Ord c) => Ms.Map c c -> [[c]] -> (Ms.Map c c, [[c]])
eqlizeBody = loop where
    loop m [] = (m, [])
    loop m (t : ts) = let (m1, t')  = eqlizeList m t
                          (m2, ts') = loop m1 ts
                      in (m2, t' : ts')


-- ----------------------  dump-tree

-- | __dump-tree E__
consDumpTree :: (K.CDec c) => C.RopCons c
consDumpTree med =
  do trees <- Rop.getTrees med "-tree"
     Msg.dumpTrees trees

