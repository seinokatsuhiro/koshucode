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

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat          as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- | Relmap operators that handles empties.
--
--   [@both R@]
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
ropsCoxEmpty :: (K.CContent c) => [C.Rop c]
ropsCoxEmpty = Rop.rops "cox-empty"
    [ consBoth          K.& [ "both R [-share /P ... -fill E]"
                              K.& "-relmap/ . -share? -fill?" ]
    , consComposeMaybe  K.& [ "compose-maybe R [-share /P ... -fill E]"
                              K.& "-relmap/ . -share? -fill?" ]
    , consMaybe         K.& [ "maybe R [-share /P ... -fill E]"
                              K.& "-relmap/ . -share? -fill?" ]
    ]


-- ----------------------  both

-- | __both R -share \/P ... -fill E__
consBoth :: (K.CContent c) => C.RopCons c
consBoth med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapBoth med sh fill rmap

-- | Create @both@ relmap.
relmapBoth :: (Ord c, K.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> K.Map (C.Relmap c)
relmapBoth med sh fill rmap = C.relmapCopy med "i" rmapBoth where
    rmapBoth = rmapL K.++ Rop.relmapJoin med sh rmapR
    rmapR    = rmap  K.++ relmapMaybe med sh fill rmapIn
    rmapL    = relmapMaybe med sh fill rmap
    rmapIn   = C.relmapLocalSymbol med "i"


-- ----------------------  maybe

-- | __maybe R -share \/P ... -fill E__
--
--   Meet input and given relation.
--   It keeps input tuples of which counterparts are totally negated.
--
consMaybe :: (K.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapMaybe med sh fill rmap

-- | Create @maybe@ relmap.
relmapMaybe :: (Ord c, K.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> K.Map (C.Relmap c)
relmapMaybe med sh = C.relmapBinary med . relkitMaybe sh

-- | Create @maybe@ relkit.
relkitMaybe :: forall c. (Ord c, K.CRel c) => Rop.SharedTerms -> c -> C.RelkitBinary c
relkitMaybe sh fill (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    lr   = K.termPicker he1 he2
    he3  = he2 K.++ he1
    kit3 = case Rop.unmatchShare sh lr of
             Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
             Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = K.gatherToMap $ map (K.pkRSplit lr) bo2
           Right $ step b2map `concatMap` bo1

    heFill = K.pkRProper lr $ K.headTypes he2
    fills  = selectFiller fill `map` heFill
    step b2map cs1 = case K.lookupMap (K.pkLShare lr cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [fills ++ cs1]

relkitMaybe _ _ _ _ = Right C.relkitNothing

selectFiller :: (K.CRel c) => c -> K.Type -> c
selectFiller _ t@(K.TypeRel _) = K.pRel $ K.Rel (K.Head t) []
selectFiller fill _ = fill


-- ----------------------  compose-maybe

-- | __compose-maybe R -share \/P ... -fill E__
--
--  Construct relmap for relational composition.
--
consComposeMaybe :: (K.CContent c) => C.RopCons c
consComposeMaybe med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapComposeMaybe med sh fill rmap

-- | Create @compose-maybe@ relmap.
relmapComposeMaybe :: (Ord c, K.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> K.Map (C.Relmap c)
relmapComposeMaybe med sh fill = C.relmapBinary med $ Rop.relkitCompose m sh where
    m sh2 = relkitMaybe sh2 fill

