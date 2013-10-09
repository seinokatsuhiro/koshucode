{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Binary
( 
  -- * maybe
  ropConsMaybe, relmapMaybe, relfyMaybe,
  -- * full
  ropConsFull, relmapFull, relfyFull,
  -- * group
  ropConsGroup, relmapGroup, relfyGroup,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal as ROP
import Koshucode.Baala.Vanilla.Type



-- ----------------------  maybe

ropConsMaybe :: C.RopCons VContent
ropConsMaybe use =
    do m <- Builtin.getRelmap use
       Right $ relmapMaybe use m

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use m = C.relmapConfl use "maybe" fy [m] where
    fy [r2] = relfyMaybe r2
    fy _    = B.bug

relfyMaybe :: (Ord c, C.CNil c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyMaybe (C.Relfy h2 f2) h1 =
    Right $ C.Relfy h3 (C.RelfyAbFull False f3)
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

      m2 b1   = do b2 <- C.relfy f2 b1
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

ropConsFull :: C.RopCons VContent
ropConsFull use =
    do [m1, m2] <- Builtin.getRelmaps use
       Right $ relmapFull use m1 m2

{-| like SQL's full join -}
relmapFull :: (Ord c, C.CNil c) => C.RopUse c
           -> C.Relmap c -> C.Relmap c -> C.Relmap c
relmapFull use m1 m2 = C.relmapConfl use "full" fy [m1, m2] where
    fy [r1, r2] = relfyFull r1 r2
    fy _ = B.bug

relfyFull
    :: (Ord c, C.CNil c)
    => C.Relfy c -> C.Relfy c
    -> B.Relhead -> B.Ab (C.Relfy c)
relfyFull (C.Relfy h1 f1) (C.Relfy h2 f2) _ = 
    do C.Relfy h3 f3 <- relfyMaybe (C.Relfy h2 f2) h1
       C.Relfy h4 f4 <- relfyMaybe (C.Relfy h1 f1) h2
       b1 <- C.relfy f1 []
       b2 <- C.relfy f2 []
       b3 <- C.relfy f3 b1
       b4 <- C.relfy f4 b2
       C.Relfy h5 f5 <- ROP.relfyJoin (C.Relfy h4 $ C.RelfyConst b4) h3
       b5 <- C.relfy f5 b3
       Right $ C.Relfy h5 (C.RelfyConst b5)



-- ----------------------  group

ropConsGroup :: C.RopCons VContent
ropConsGroup use =
  do n <- Builtin.getTerm   use "-term"
     m <- Builtin.getRelmap use
     Right $ relmapGroup use n m

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use n m = C.relmapConfl use "group" fy [m] where
    fy [r2] = relfyGroup n r2
    fy _    = B.bug

{-| Grouping relation. -}
relfyGroup :: (Ord c, C.CRel c) => String -> C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyGroup n (C.Relfy h2 f2) h1 =
    Right $ C.Relfy h3 (C.RelfyAbFull False f3)
    where
      shared    :: [B.Termname]
      shared    =  B.posInnerNames $ h1 `B.posFrom` h2

      share1, share2 :: [B.TermPos]
      share1    =  h1 `B.posFor` shared
      share2    =  h2 `B.posFor` shared

      m2 b1     = do b2 <- C.relfy f2 b1
                     Right $ B.gatherToMap $ map kv b2
      kv cs2    = ( B.posPick share2 cs2, cs2 )

      h3        = B.Relhead $ B.Nest n (B.headTerms h2) : (B.headTerms h1)
      f3 b1     = do m <- m2 b1
                     Right $ map (step m) b1
      step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                     Just cs2' -> (C.putRel $ B.Rel h2 cs2') : cs1
                     Nothing     -> []

