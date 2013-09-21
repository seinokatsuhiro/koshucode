{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Minimal.Term
( -- * pick
  ropConsPick, relmapPick, relfyPick,
  -- * cut
  ropConsCut, relmapCut, relfyCut,
  -- * rename
  ropConsRename, relmapRename, relfyRename
) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin



-- ----------------------  pick

ropConsPick :: (Ord c) => C.RopCons c
ropConsPick use =
  do ns <- getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapPick use ns = C.relmapCalc use "pick" fy where
    fy _ = relfyPick ns

relfyPick
    :: [String]            -- ^ Names of picking terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)    -- ^ Relfier for output relation
relfyPick ns = relfyArrange B.arrangePick B.arrangePick ns



-- ----------------------  cut

ropConsCut :: (Ord c) => C.RopCons c
ropConsCut use =
  do ns <- getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapCut use ns = C.relmapCalc use "cut" fy where
    fy _ = relfyCut ns

relfyCut
    :: [String]            -- ^ Names of cutting terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)    -- ^ Relfier for output relation
relfyCut ns = relfyArrange B.arrangeCut B.arrangeCut ns

relfyArrange
    :: B.Arrange String
    -> B.Arrange c
    -> [String]
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
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(String, String)] -> C.Relmap c
relmapRename use np = C.relmapCalc use "rename" fy where
    fy _ = relfyRename np

{-| Change terms names -}
relfyRename
    :: [(String, String)]  -- ^ List of termnames (/to/, /from/)
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)    -- ^ Relfier for output relation
relfyRename np h1
    | nsCheck /= [] = Left  $ B.AbortReqNewTerms nsCheck
    | psCheck /= [] = Left  $ B.AbortNoTerms psCheck
    | otherwise     = Right $ C.Relfy h2 C.RelfyId
    where
      (ns, ps) = unzip np
      nsCheck  = B.headExistTerms    h1 ns
      psCheck  = B.headNonExistTerms h1 ps
      h2       = B.headChange (map ren) h1
      pn       = map Tuple.swap np
      ren p    = Maybe.fromMaybe p $ lookup p pn

