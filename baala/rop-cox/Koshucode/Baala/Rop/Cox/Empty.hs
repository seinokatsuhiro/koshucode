{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Relational compositions whose contents are maybe the empty.

module Koshucode.Baala.Rop.Cox.Empty
  ( ropsCoxEmpty,
    -- * both
    consBoth, relmapBoth,
    -- * maybe
    consMaybe, relmapMaybe, relkitMaybe,
    -- * compose-maybe
    consComposeMaybe, relmapComposeMaybe,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat          as Rop
import qualified Koshucode.Baala.Rop.Cox.Get       as Rop
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
ropsCoxEmpty = Rop.ropList "cox-empty"  -- GROUP
    --        CONSTRUCTOR USAGE                              ATTRIBUTE
    [ Rop.def consBoth    "both R [-share /P ... -fill E]"   "-relmap/ . -share? -fill?"
    , Rop.def consComposeMaybe  "compose-maybe R [-share /P ... -fill E]"
                                                             "-relmap/ . -share? -fill?"
    , Rop.def consMaybe   "maybe R [-share /P ... -fill E]"  "-relmap/ . -share? -fill?"
    ]


-- ----------------------  both

-- | __both R -share \/P ... -fill E__
consBoth :: (D.CContent c) => C.RopCons c
consBoth med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapBoth med sh fill rmap

-- | Create @both@ relmap.
relmapBoth :: (Ord c, D.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> O.Map (C.Relmap c)
relmapBoth med sh fill rmap = C.relmapCopy med "i" rmapBoth where
    rmapBoth = rmapL B.<> Rop.relmapJoin med sh rmapR
    rmapR    = rmap  B.<> relmapMaybe med sh fill rmapIn
    rmapL    = relmapMaybe med sh fill rmap
    rmapIn   = C.relmapLocalSymbol med "i"


-- ----------------------  maybe

-- | Construct maybe relmap.
consMaybe :: (D.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapMaybe med sh fill rmap

-- | Create @maybe@ relmap.
relmapMaybe :: (Ord c, D.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> O.Map (C.Relmap c)
relmapMaybe med sh = C.relmapBinary med . relkitMaybe sh

-- | Create @maybe@ relkit.
relkitMaybe :: forall c. (Ord c, D.CRel c) => Rop.SharedTerms -> c -> C.RelkitBinary c
relkitMaybe sh fill (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr   = D.termPicker he1 he2
    he3  = he2 B.<> he1
    kit3 = case Rop.unmatchShare sh lr of
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

-- | __compose-maybe__
--
---  Construct relmap for relational composition.
--
--   >>> a | compose-maybe b

consComposeMaybe :: (D.CContent c) => C.RopCons c
consComposeMaybe med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapComposeMaybe med sh fill rmap

-- | Create @compose-maybe@ relmap.
relmapComposeMaybe :: (Ord c, D.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> O.Map (C.Relmap c)
relmapComposeMaybe med sh fill = C.relmapBinary med $ Rop.relkitCompose m sh where
    m sh2 = relkitMaybe sh2 fill

