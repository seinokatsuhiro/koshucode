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

relkitMaybe :: (Ord c, C.CNil c) => C.RelkitBinary c
relkitMaybe (C.Relkit h2 f2) h1 =
    Right $ C.relkit h3 (C.RelkitAbFull False f3)
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

      m2 b1   = do b2 <- C.relkitRun f2 b1
                   Right $ B.gatherToMap $ map kv b2
      kv cs2  = ( B.posPick share2 cs2,
                  B.posPick side2  cs2 )

      h3 = B.mappend h2 h1
      f3 b1 = do m <- m2 b1
                 Right $ concatMap (step m) b1
      nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
      step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                     Just side -> map (++ cs1) side
                     Nothing   -> [nils ++ cs1]



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

{-| Grouping relation. -}
relkitGroup :: forall c. (Ord c, C.CRel c) => String -> C.RelkitBinary c
relkitGroup n (C.Relkit h2 f2) h1 =
    Right $ C.relkit h3 (C.RelkitAbFull False f3)
    where
      shared    :: [B.Termname]
      shared    = B.posInnerNames $ h1 `B.posFrom` h2

      share1, share2 :: [B.TermPos]
      share1    = h1 `B.posFor` shared
      share2    = h2 `B.posFor` shared

      toMap2 b1 = do b2 <- C.relkitRun f2 b1
                     Right $ B.gatherToMap $ map kv b2
      kv cs2    = ( B.posPick share2 cs2, cs2 )

      h3        = B.Nest n (B.headTerms h2) `B.headConsTerm` h1
      f3 b1     = do map2 <- toMap2 b1
                     Right $ map (add map2) b1
      add map2 cs1 =
          let b2maybe = B.lookupMap (B.posPick share1 cs1) map2
              b2sub   = maybe [] id b2maybe
          in C.pRel (B.Rel h2 b2sub) : cs1



-- ----------------------  if

consIf :: (Ord c) => C.RopCons c
consIf use =
  do rs <- Rop.getRelmaps use
     Right $ relmapIf use rs

relmapIf :: (Ord c) => C.RopUse c -> [C.Relmap c] -> C.Relmap c
relmapIf use = C.relmapConfl use relkitIf

relkitIf :: forall c. (Ord c) => C.RelkitConfl c
relkitIf [C.Relkit _ ft, C.Relkit hc fc, C.Relkit ha fa] _
    | hc == ha = Right $ C.relkit hc (C.RelkitAbFull True f3) where
    f3 b1 = do b2 <- C.relkitRun ft b1
               case b2 of
                 [] -> C.relkitRun fa b1
                 _  -> C.relkitRun fc b1
relkitIf _ _ = B.bug "relkitIf"

