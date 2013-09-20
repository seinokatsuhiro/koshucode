{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Unary
( -- * reldee & reldum
  ropConsReldee, ropConsReldum,
  -- * source
  ropConsSource,
  -- * id
  ropConsId, relmapId,
  -- * empty
  ropConsEmpty, relmapEmpty, relfyEmpty,
  -- * pick
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



-- ----------------------  reldee & reldum

ropConsReldee, ropConsReldum :: C.RopCons c
ropConsReldee use = Right $ C.relmapConst use "reldee" B.reldee
ropConsReldum use = Right $ C.relmapConst use "reldum" B.reldum


-- ----------------------  source

ropConsSource :: C.RopCons c
ropConsSource use =
  do sign <- getWord  use "-sign"
     ns   <- getTerms use "-term"
     Right $ C.relmapSource use sign ns


-- ----------------------  id

ropConsId :: C.RopCons c
ropConsId use = Right $ relmapId use

{-| Identity mapping, i.e., do nothing. -}
relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapCalc use "id" C.relfyId where



-- ----------------------  empty

ropConsEmpty :: C.RopCons c
ropConsEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapCalc use "empty" fy where
    fy _ = relfyEmpty

{-| Throw away all tuples in a relation. -}
relfyEmpty :: B.Relhead -> B.Ab (C.Relfy c)
relfyEmpty h1 = Right $ C.Relfy h1 (C.RelfyConst [])



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

