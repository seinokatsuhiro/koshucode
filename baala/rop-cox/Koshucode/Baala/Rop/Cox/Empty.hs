{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Relational compositions whose contents are maybe the empty.

module Koshucode.Baala.Rop.Cox.Empty
  ( ropsCoxEmpty,
    -- * both
    consBoth, relmapBoth,
    -- * maybe
    consMaybe, relmapMaybe, relkitMaybe,
    -- * maybe with restriction
    MeetOnly, RelPortion (..),
    relmapMaybeOnly,
    -- * compose-maybe
    consComposeMaybe, relmapComposeMaybe,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- | Relmap operators that handles empties.
ropsCoxEmpty :: (K.CContent c) => [C.Rop c]
ropsCoxEmpty = Rop.rops "cox-empty"
    [ consBoth
      K.& [ "both R [-fill E] [-share /P ...]"
            K.& "-relmap/ . -fill? -share?" ]
    , consComposeMaybe
      K.& [ "compose-maybe R [-fill E] [-share /P ...]"
            K.& "-relmap/ . -fill? -share?" ]
    , consMaybe
      K.& [ "maybe R [-fill E] [-share /P ...]"
            K.& "plain : -relmap/ . -fill? -share?"
          , "maybe R -only E [-fill E] [-share /P ...]"
            K.& "only : -relmap/ . -only -fill? -share?"
          , "maybe R -only E -order /P ... [-top N] [-mid N] [-bot N] [-fill E] [-share /P ...]"
            K.& "order : -relmap/ . -only -order -top? -mid? -bot? -fill? -share?"
          , "maybe R -only E -order /P ... -part N -of N [-fill E] [-share /P ...]"
            K.& "part : -relmap/ . -only -order -part -of -fill? -share?" ]
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

-- | [maybe /R/ -share /\/P/ ... -fill /E/]
--     Meet input and given relation /R/.
--     It keeps input tuples of which counterparts are totally negated.
--   [maybe /R/ -only /E/ -order /\/P/ ... -top /N/ -mid /N/ -bot /N/]
--     Maybe with restriction by /E/ and limit to /N/ tuples.
--   [maybe /R/ -only /E/ -order /\/P/ ... -part /N/ -of /N/]
--     Maybe with restriction by /E/ and take /N/'th part.
--
consMaybe :: (K.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Rop.getRelmap med "-relmap"
       fill <- Rop.getFiller med "-fill"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       case Rop.getTags med of
         tags | "only" `elem` tags
                  -> maybeOnly sh fill rmap $ Right K.def

              | "order" `elem` tags
                  -> maybeOnly sh fill rmap $ do
                        order <- Rop.getTerms med "-order"
                        top   <- Rop.getMaybe Rop.getInt med "-top"
                        mid   <- Rop.getMaybe Rop.getInt med "-mid"
                        bot   <- Rop.getMaybe Rop.getInt med "-bot"
                        Right $ K.def { portionOrder   = order
                                      , portionTop     = fromInteger <$> top
                                      , portionMiddle  = fromInteger <$> mid
                                      , portionBottom  = fromInteger <$> bot }

              | "part" `elem` tags
                  -> maybeOnly sh fill rmap $ do
                        order <- Rop.getTerms med "-order"
                        part  <- Rop.getInt med "-part"
                        per   <- Rop.getInt med "-of"
                        return $ K.def { portionOrder   = order
                                       , portionParts   = [fromInteger part - 1]
                                       , portionPer     = Just $ fromInteger per }

              | otherwise -> Right $ relmapMaybe med sh fill rmap

    where
      maybeOnly sh fill rmap createPortion =
          do cops  <- Rop.getLet med "-let"
             cox   <- Rop.getCox med "-only"
             portion <- createPortion
             Right $ relmapMaybeOnly med ((sh, cops, cox, portion), fill) rmap

-- | Create @maybe@ relmap.
relmapMaybe :: (Ord c, K.CRel c) => C.Intmed c -> Rop.SharedTerms -> c -> K.Map (C.Relmap c)
relmapMaybe med sh = C.relmapBinary med . relkitMaybe sh

-- | Create @maybe@ relkit.
relkitMaybe :: forall c. (Ord c, K.CRel c) => Rop.SharedTerms -> c -> C.RelkitBinary c
relkitMaybe sh fill (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    pk     = K.termPicker he1 he2
    pick1  = K.pkLShare pk
    cut2   = K.pkRProper pk
    split2 = K.pkRSplit pk

    he3  = he2 K.++ he1
    kit3 = case Rop.unmatchShare sh pk of
             Nothing     -> Right $ C.relkitConflWhole False he3 f [kitb2]
             Just (e, a) -> Msg.unmatchShare e a

    f :: [C.BodyMap c] -> C.BodyMap c
    f bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = K.gatherToMap (split2 <$> bo2)
           Right $ step b2map K.<++> bo1

    heFill = cut2 $ K.headTypes he2
    fills  = selectFiller fill <$> heFill
    step b2map cs1 = case pick1 cs1 `K.lookupMap` b2map of
                       Just b2prop -> map (++ cs1) b2prop
                       Nothing     -> [fills ++ cs1]

relkitMaybe _ _ _ _ = C.relkitUnfixed

selectFiller :: (K.CRel c) => c -> K.Type -> c
selectFiller _ t@(K.TypeRel _) = K.pRel $ K.Rel (K.Head t) []
selectFiller fill _ = fill


-- ----------------------  maybe with restriction

-- | Parameter for @meet -only@.
type MeetOnly c = (Rop.SharedTerms, K.CopSet c, K.Cox c, RelPortion)

-- | Create @maybe@-with-restriction relmap.
relmapMaybeOnly :: (K.CContent c) => C.Intmed c -> (MeetOnly c, c) -> K.Map (C.Relmap c)
relmapMaybeOnly med p = C.relmapBinary med $ relkitMaybeOnly p

-- | Create @maybe@-with-restriction relkit.
relkitMaybeOnly :: forall c. (K.CContent c) => (MeetOnly c, c) -> C.RelkitBinary c
relkitMaybeOnly ((sh, cops, cox, portion), fill) (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    pk     = K.termPicker he1 he2
    pick1  = K.pkLShare pk
    cut2   = K.pkRProper pk
    split2 = K.pkRSplit pk

    he3  = he2 K.++ he1
    kit3 = case Rop.unmatchShare sh pk of
             Nothing     -> Right $ C.relkitConflWhole False he3 f [kitb2]
             Just (e, a) -> Msg.unmatchShare e a

    f :: [C.BodyMap c] -> C.BodyMap c
    f bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = K.gatherToMap (split2 <$> bo2)
           step b2map K.<#++> bo1

    heFill = cut2 $ K.headTypes he2
    fills  = selectFiller fill <$> heFill
    step b2map cs1 =
        case pick1 cs1 `K.lookupMap` b2map of
          Nothing     -> Right [fills ++ cs1]
          Just b2prop -> do bo3 <- K.filterM keep ((++ cs1) <$> b2prop)
                            case bo3 of
                              [] -> Right [fills ++ cs1]
                              _  -> Right $ applyPortion bo3

    keep cs = do c <- K.calcCox cops he3 cs cox
                 case K.isBool c of
                   True  -> Right $ K.gBool c
                   False -> Msg.reqBool

    applyPortion | isEffectivePortion portion = relPortion portion he3
                 | otherwise = id

relkitMaybeOnly _ _ _ = C.relkitUnfixed

-- | Portion of relation.
data RelPortion = RelPortion
    { portionOrder    :: [K.TermName] -- ^ Order terms.
    , portionTop      :: Maybe Int    -- ^ Top /N/ tuples.
    , portionMiddle   :: Maybe Int    -- ^ Middle /N/ tuples.
    , portionBottom   :: Maybe Int    -- ^ Bottom /N/ tuples.
    , portionParts    :: [Int]        -- ^ Indecies of /N/ chunks.
    , portionPer      :: Maybe Int    -- ^ Divided /N/ chunks.
    } deriving (Show, Eq, Ord)

instance K.Default RelPortion where
    def = RelPortion { portionOrder   = []
                     , portionTop     = Nothing
                     , portionMiddle  = Nothing
                     , portionBottom  = Nothing
                     , portionParts   = []
                     , portionPer     = Nothing }

isEffectivePortion :: RelPortion -> Bool
isEffectivePortion RelPortion { portionTop     = Nothing
                              , portionMiddle  = Nothing
                              , portionBottom  = Nothing
                              , portionPer     = Nothing } = False
isEffectivePortion _ = True

-- | Take portion of relation.
relPortion :: (K.GetTermNames he, Ord c) => RelPortion -> he -> [[c]] -> [[c]]
relPortion p@RelPortion { portionOrder = ord } he body = body' where
    body' = K.unique (top p ++ mid p ++ bot p ++ parts p)
    bodyOrd = K.relBodyOrder ord he body

    top RelPortion { portionTop = Just n } = take n bodyOrd
    top _ = []

    mid RelPortion { portionMiddle = Just n } = K.takeMiddle n bodyOrd
    mid _ = []

    bot RelPortion { portionBottom = Just n } = K.takeEnd n bodyOrd
    bot _ = []

    parts RelPortion { portionParts = ps, portionPer = Just n } =
        concat $ K.takeChunks n ps bodyOrd
    parts _ = []


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
