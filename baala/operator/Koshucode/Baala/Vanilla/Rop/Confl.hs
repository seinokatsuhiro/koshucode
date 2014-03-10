{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Confl
( 
  -- * maybe
  consMaybe, relmapMaybe, relkitMaybe,
  -- * full
  consFull, relmapFull, relkitFull,
  -- * group
  consGroup, relmapGroup, relkitGroup,
  -- * if
  consIf, relmapIf, relkitIf,
  -- * when & unless
  consWhen, consUnless,
  -- * fix & fix-join
  consFix,
  consFixJoin,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal as Rop



-- ----------------------  maybe

consMaybe :: (Ord c, C.CNil c) => C.RopCons c
consMaybe use =
    do m <- Rop.getRelmap use
       Right $ relmapMaybe use m

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use = C.relmapBinary use relkitMaybe

relkitMaybe :: forall c. (Ord c, C.CNil c) => C.RelkitBinary c
relkitMaybe (C.Relkit (Just h2) f2) (Just h1) =
    Right $ C.relkitJust h3 (C.RelkitAbFull False f3 [f2])
    where
      pos     :: [B.TermPos]
      pos     =  h1 `B.posFrom` h2

      shared, sided :: [B.Termname]
      shared  =  B.posInnerNames pos
      sided   =  B.posOuterNames pos

      share1, share2, side2 :: [B.TermPos]
      share1  =  h1 `B.posFor` shared
      share2  =  h2 `B.posFor` shared
      side2   =  h2 `B.posFor` sided

      m2 b2   = Right $ B.gatherToMap $ map kv b2
      kv cs2  = ( B.posPick share2 cs2,
                  B.posPick side2  cs2 )

      h3 = B.mappend h2 h1

      f3 :: [C.Relfun c] -> C.Relfun c
      f3 sub b1 = do let [g2] = sub
                     b2 <- g2 b1
                     m  <- m2 b2
                     Right $ concatMap (step m) b1
      nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
      step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                     Just side -> map (++ cs1) side
                     Nothing   -> [nils ++ cs1]
relkitMaybe _ _ = Right C.relkitNothing


-- ----------------------  full

consFull :: (Ord c, C.CNil c) => C.RopCons c
consFull use =
    do [m1, m2] <- Rop.getRelmaps use
       Right $ relmapFull use m1 m2

{-| like SQL's full join -}
relmapFull :: (Ord c, C.CNil c) => C.RopUse c
           -> C.Relmap c -> C.Relmap c -> C.Relmap c
relmapFull use m1 m2 = C.relmapConfl use fy [m1, m2] where
    fy [r1, r2] = relkitFull r1 r2
    fy _ = B.bug "relmapFull"

relkitFull :: (Ord c, C.CNil c) => C.Relkit c -> C.RelkitBinary c
relkitFull (C.Relkit h1 f1) (C.Relkit h2 f2) _ = 
    do C.Relkit h3 f3 <- relkitMaybe (C.Relkit h2 f2) h1
       C.Relkit h4 f4 <- relkitMaybe (C.Relkit h1 f1) h2
       b1 <- C.relkitRun f1 []
       b2 <- C.relkitRun f2 []
       b3 <- C.relkitRun f3 b1
       b4 <- C.relkitRun f4 b2
       C.Relkit h5 f5 <- Rop.relkitJoin (C.relkit h4 $ C.RelkitConst b4) h3
       b5 <- C.relkitRun f5 b3
       Right $ C.relkit h5 (C.RelkitConst b5)



-- ----------------------  group

consGroup :: (Ord c, C.CRel c) => C.RopCons c
consGroup use =
  do n <- Rop.getTerm   use "-term"
     m <- Rop.getRelmap use
     Right $ relmapGroup use n m

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use = C.relmapBinary use . relkitGroup

-- | Grouping relation.
relkitGroup :: forall c. (Ord c, C.CRel c) => String -> C.RelkitBinary c
relkitGroup n (C.Relkit (Just h2) f2) (Just h1) =
    Right $ C.relkitJust h3 (C.RelkitAbFull False f3 [f2])
    where
      shared    :: [B.Termname]
      shared    = B.posInnerNames $ h1 `B.posFrom` h2

      share1, share2 :: [B.TermPos]
      share1    = h1 `B.posFor` shared
      share2    = h2 `B.posFor` shared

      toMap2 b2 = Right $ B.gatherToMap $ map kv b2
      kv cs2    = ( B.posPick share2 cs2, cs2 )

      h3        = B.Nest n (B.headTerms h2) `B.headConsTerm` h1
      f3 sub b1 = do let [g2] = sub
                     b2 <- g2 b1
                     map2 <- toMap2 b2
                     Right $ map (add map2) b1
      add map2 cs1 =
          let b2maybe = B.lookupMap (B.posPick share1 cs1) map2
              b2sub   = B.maybeWith [] b2maybe
          in C.pRel (B.Rel h2 b2sub) : cs1
relkitGroup _ _ _ = Right C.relkitNothing



-- ----------------------  if

--  'if T A B' is same as 'A' when T is an empty relation
--  or same as 'B' when T is not an empty relation.

consIf :: (Ord c) => C.RopCons c
consIf use =
  do rs <- Rop.getRelmaps use
     Right $ relmapIf use rs

relmapIf :: (Ord c) => C.RopUse c -> [C.Relmap c] -> C.Relmap c
relmapIf use = C.relmapConfl use relkitIf

relkitIf :: forall c. (Ord c) => C.RelkitConfl c
relkitIf [rt@(C.Relkit _ ft), ra@(C.Relkit ha fa), rb@(C.Relkit hb fb)] _
    | isNothing2 ha hb  = Right C.relkitNothing
    | isNothing ha      = relkitIf [rt, C.Relkit hb fa, rb] Nothing
    | isNothing hb      = relkitIf [rt, ra, C.Relkit ha fb] Nothing
    | otherwise         =
        case (ha, hb) of
          (Just a, Just b) | B.headEquiv a b
            -> Right $ C.relkit ha (C.RelkitAbFull True (f3 a b) subkits)
          _ -> Left $ B.abortOperand $ "different headings: "
               ++ showHead ha ++ " and " ++ showHead hb
    where
      showHead h = show (B.doc $ maybe (B.headFrom []) id h)
      subkits  = [ft, fa, fb]
      f3 c a sub b1 = do let [gt, gc, ga] = sub
                         bt <- gt b1
                         if bt /= [] then gc b1 else align c a (ga b1)

      align :: B.Relhead -> B.Relhead -> B.Map (B.Ab [[c]])
      align a b = fmap (B.headArrange a b `map`)
relkitIf _ _ = Left $ B.abortOperand $ "if T A b"

isNothing :: Maybe B.Relhead -> Bool
isNothing = (== Nothing)

isNothing2 :: Maybe B.Relhead -> Maybe B.Relhead -> Bool
isNothing2 a b = isNothing a && isNothing b



-- ----------------------  when & unless

consWhen :: (Ord c) => C.RopCons c
consWhen use =
  do [test, con] <- Rop.getRelmaps use
     Right $ relmapIf use [test, con, C.relmapId]

consUnless :: (Ord c) => C.RopCons c
consUnless use =
  do [test, alt] <- Rop.getRelmaps use
     Right $ relmapIf use [test, C.relmapId, alt]



-- ----------------------  fix & fix-join

consFix :: (Ord c) => C.RopCons c
consFix use =
  do r <- Rop.getRelmap use
     Right $ relmapFix use r

consFixJoin :: (Ord c) => C.RopCons c
consFixJoin use =
  do r <- Rop.getRelmap use
     Right $ relmapFix use (Rop.relmapJoin use r)

relmapFix :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapFix use = C.relmapBinary use relkitFix

relkitFix :: forall c. (Ord c) => C.RelkitBinary c
relkitFix (C.Relkit (Just h2) f2) (Just h1) =
    Right $ C.relkitJust h1 (C.RelkitAbFull True f3 [f2])
    where f3 sub = let [g] = sub
                       g'  = C.bodyMapArrange h1 h2 g
                   in C.fixedRelation g'
relkitFix _ _ = Right C.relkitNothing

