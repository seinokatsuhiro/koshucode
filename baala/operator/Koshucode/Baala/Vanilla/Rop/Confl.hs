{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Confl
( 
  -- * maybe
  ropConsMaybe, relmapMaybe, relfyMaybe,
  -- * full
  ropConsFull, relmapFull, relfyFull,
  -- * group
  ropConsGroup, relmapGroup, relfyGroup,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal as Rop



-- ----------------------  maybe

ropConsMaybe :: (Ord c, C.CNil c) => C.RopCons c
ropConsMaybe use =
    do m <- Rop.getRelmap use
       Right $ relmapMaybe use m

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use m = C.relmapConfl use fy [m] where
    fy [r2] = relfyMaybe r2
    fy _    = B.bug

relfyMaybe :: (Ord c, C.CNil c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyMaybe (C.Relfy h2 f2) h1 =
    Right $ C.relfy h3 (C.RelfyAbFull False f3)
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

      m2 b1   = do b2 <- C.relfyRun f2 b1
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

ropConsFull :: (Ord c, C.CNil c) => C.RopCons c
ropConsFull use =
    do [m1, m2] <- Rop.getRelmaps use
       Right $ relmapFull use m1 m2

{-| like SQL's full join -}
relmapFull :: (Ord c, C.CNil c) => C.RopUse c
           -> C.Relmap c -> C.Relmap c -> C.Relmap c
relmapFull use m1 m2 = C.relmapConfl use fy [m1, m2] where
    fy [r1, r2] = relfyFull r1 r2
    fy _ = B.bug

relfyFull
    :: (Ord c, C.CNil c)
    => C.Relfy c -> C.Relfy c
    -> B.Relhead -> B.Ab (C.Relfy c)
relfyFull (C.Relfy h1 f1) (C.Relfy h2 f2) _ = 
    do C.Relfy h3 f3 <- relfyMaybe (C.Relfy h2 f2) h1
       C.Relfy h4 f4 <- relfyMaybe (C.Relfy h1 f1) h2
       b1 <- C.relfyRun f1 []
       b2 <- C.relfyRun f2 []
       b3 <- C.relfyRun f3 b1
       b4 <- C.relfyRun f4 b2
       C.Relfy h5 f5 <- Rop.relfyJoin (C.relfy h4 $ C.RelfyConst b4) h3
       b5 <- C.relfyRun f5 b3
       Right $ C.relfy h5 (C.RelfyConst b5)



-- ----------------------  group

ropConsGroup :: (Ord c, C.CRel c) => C.RopCons c
ropConsGroup use =
  do n <- Rop.getTerm   use "-term"
     m <- Rop.getRelmap use
     Right $ relmapGroup use n m

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use n m = C.relmapConfl use fy [m] where
    fy [r2] = relfyGroup n r2
    fy _    = B.bug

{-| Grouping relation. -}
relfyGroup :: (Ord c, C.CRel c) => String -> C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyGroup n (C.Relfy h2 f2) h1 =
    Right $ C.relfy h3 (C.RelfyAbFull False f3)
    where
      shared    :: [B.Termname]
      shared    = B.posInnerNames $ h1 `B.posFrom` h2

      share1, share2 :: [B.TermPos]
      share1    = h1 `B.posFor` shared
      share2    = h2 `B.posFor` shared

      toMap2 b1 = do b2 <- C.relfyRun f2 b1
                     Right $ B.gatherToMap $ map kv b2
      kv cs2    = ( B.posPick share2 cs2, cs2 )

      h3        = B.Nest n (B.headTerms h2) `B.headConsTerm` h1
      f3 b1     = do map2 <- toMap2 b1
                     Right $ map (add map2) b1
      add map2 cs1 =
          let b2maybe = B.lookupMap (B.posPick share1 cs1) map2
              b2sub   = maybe [] id b2maybe
          in C.putRel (B.Rel h2 b2sub) : cs1

