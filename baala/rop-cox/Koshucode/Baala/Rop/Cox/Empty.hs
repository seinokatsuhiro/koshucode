{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Cox.Empty
  ( ropsCoxEmpty,
    -- * both
    consBoth, relmapBoth,
    -- * maybe
    consMaybe, relmapMaybe, relkitMaybe,
    -- * compose-maybe
    consComposeMaybe, relmapComposeMaybe,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Op
import qualified Koshucode.Baala.Rop.Flat          as Op
import qualified Koshucode.Baala.Rop.Cox.Get       as Op
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Relmap operators that handles empties.
--
--   [@both R@]
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
ropsCoxEmpty :: (D.CContent c) => [C.Rop c]
ropsCoxEmpty = Op.ropList "cox-empty"  -- GROUP
    --        CONSTRUCTOR USAGE                              ATTRIBUTE
    [ Op.def  consBoth    "both R [-share /P ... -fill E]"   "-relmap/ . -share? -fill?"
    , Op.def  consComposeMaybe  "compose-maybe R [-share /P ... -fill E]"
                                                             "-relmap/ . -share? -fill?"
    , Op.def  consMaybe   "maybe R [-share /P ... -fill E]"  "-relmap/ . -share? -fill?"
    ]


-- ----------------------  both

consBoth :: (D.CContent c) => C.RopCons c
consBoth med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapBoth med sh fill rmap

relmapBoth :: (Ord c, D.CRel c) => C.Intmed c -> Op.SharedTerms -> c -> B.Map (C.Relmap c)
relmapBoth med sh fill rmap = C.relmapCopy med "i" rmapBoth where
    rmapBoth = rmapL B.<> Op.relmapJoin med sh rmapR
    rmapR    = rmap  B.<> relmapMaybe med sh fill rmapIn
    rmapL    = relmapMaybe med sh fill rmap
    rmapIn   = C.relmapLocalSymbol med "i"


-- ----------------------  maybe

-- | Construct maybe relmap.
consMaybe :: (D.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapMaybe med sh fill rmap

-- | Maybe relmap.
relmapMaybe :: (Ord c, D.CRel c) => C.Intmed c -> Op.SharedTerms -> c -> B.Map (C.Relmap c)
relmapMaybe med sh = C.relmapBinary med . relkitMaybe sh

-- | Calculate maybe relmap.
relkitMaybe :: forall c. (Ord c, D.CRel c) => Op.SharedTerms -> c -> C.RelkitBinary c
relkitMaybe sh fill (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr   = D.shareSide he1 he2
    he3  = he2 B.<> he1
    kit3 = case Op.unmatchShare sh lr of
             Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
             Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map (D.ssRSplit lr) bo2
           Right $ step b2map `concatMap` bo1

    heFill = D.ssRSide lr $ D.headTypes he2
    fills  = selectFiller fill `map` heFill
    step b2map cs1 = case B.lookupMap (D.ssLShare lr cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [fills ++ cs1]

relkitMaybe _ _ _ _ = Right C.relkitNothing

selectFiller :: (D.CRel c) => c -> D.Type -> c
selectFiller _ t@(D.TypeRel _) = D.pRel $ D.Rel (D.Head t) []
selectFiller fill _ = fill


-- ----------------------  compose-maybe

-- | Construct relmap for relational composition.
--
--   >>> a | compose-maybe b

consComposeMaybe :: (D.CContent c) => C.RopCons c
consComposeMaybe med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapComposeMaybe med sh fill rmap

-- | Relational composition.
relmapComposeMaybe :: (Ord c, D.CRel c) => C.Intmed c -> Op.SharedTerms -> c -> B.Map (C.Relmap c)
relmapComposeMaybe med sh fill = C.relmapBinary med $ Op.relkitCompose m sh where
    m sh2 = relkitMaybe sh2 fill

