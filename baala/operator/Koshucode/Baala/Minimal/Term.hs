{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Minimal.Term
( -- * pick
  ropConsPick, relmapPick,
  -- * cut
  ropConsCut, relmapCut,
  -- * rename
  ropConsRename, relmapRename,
) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  pick

ropConsPick :: (Ord c) => C.RopCons c
ropConsPick use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapPick use ns = C.relmapCalc use "pick" fy where
    fy _ = relfyPick ns

relfyPick
    :: [B.Termname]        -- ^ Names of picking terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)    -- ^ Relfier for output relation
relfyPick ns = relfyArrange B.arrangePick B.arrangePick ns



-- ----------------------  cut

ropConsCut :: (Ord c) => C.RopCons c
ropConsCut use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCut use ns = C.relmapCalc use "cut" fy where
    fy _ = relfyCut ns

relfyCut
    :: [B.Termname]        -- ^ Names of cutting terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)    -- ^ Relfier for output relation
relfyCut ns = relfyArrange B.arrangeCut B.arrangeCut ns

relfyArrange
    :: B.Arrange B.Termname
    -> B.Arrange c
    -> [B.Termname]
    -> B.Relhead
    -> B.Ab (C.Relfy c)
relfyArrange ha ba ns h1
    | null non  = Right $ C.Relfy h2 (C.RelfyOneToOne True $ ba ind)
    | otherwise = Left  $ B.AbortNoTerms non
    where
      non =  B.headNonExistTerms h1 ns
      pos :: [B.TermPos]
      pos =  List.sort $ h1 `B.posFor` ns

      ind :: [Int]
      ind =  map B.posIndex pos

      h2  =  B.headChange (ha ind) h1



-- ----------------------  rename

ropConsRename :: C.RopCons c
ropConsRename use = do
  np <- Rop.getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(B.Termname, B.Termname)] -> C.Relmap c
relmapRename use np = C.relmapCalc use "rename" fy where
    fy _ = relfyRename np

{-| Change terms names -}
relfyRename
    :: [(B.Termname, B.Termname)]  -- ^ List of termnames (/to/, /from/)
    -> B.Relhead                   -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)            -- ^ Relfier for output relation
relfyRename np h1
    | nsCheck /= [] = Left  $ B.AbortReqNewTerms nsCheck
    | psCheck /= [] = Left  $ B.AbortNoTerms psCheck
    | otherwise     = Right $ C.Relfy h2 C.RelfyId
    where
      (ns, ps) = unzip np
      nsCheck  = B.headExistTerms    h1 ns
      psCheck  = B.headNonExistTerms h1 ps
      h2       = B.headChange (map rename) h1
      pn       = map Tuple.swap np
      rename p = Maybe.fromMaybe p $ lookup p pn

