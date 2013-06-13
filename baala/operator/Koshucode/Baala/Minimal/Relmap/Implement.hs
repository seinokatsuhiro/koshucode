{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Relmap.Implement
( -- * List of operators
  -- $ListOfOperators

  -- * Implementations of operators
  minimalRelmaps,
  relmapRename,

  -- * Utilities
  abortableHead,
  signFromOperand
) where
import Data.Monoid
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Struct
import Koshucode.Baala.Base.Prelude.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary
import Koshucode.Baala.Minimal.Relmap.Operand
import qualified Koshucode.Baala.Base.Kit as Kit
import qualified Koshucode.Baala.Base.Syntax as Syn



-- ----------------------  Operators

-- | Minimal implementations of relmaps
minimalRelmaps :: (Ord v) => [RelmapImplement v]
minimalRelmaps = relmaps
    -- Relmap operators in alphabetical order
    [ o "|"      LikeNone   fullConcat ["RELMAP | RELMAP"]
    , o "cut"    LikePick   fullCut    ["cut /NAME ..."]
    , o "empty"  LikeNone   fullEmpty  ["empty"]
    , o "join"   LikeMeet   fullJoin   ["join RELMAP"]
    , o "meet"   LikeMeet   fullMeet   ["meet RELMAP"]
    , o "pick"   LikePick   fullPick   ["pick /NAME ..."]
    , o "reldee" LikeConst  fullReldee ["reldee"]
    , o "reldum" LikeConst  fullReldum ["reldum"]
    , o "rename" LikeRename fullRename ["rename /NEW /OLD ..."]
    , o "source" LikeSource fullSource ["source SIGN /NAME ..."]
    ] where o = (,,,)

fullConcat :: RelmapFullCons v
fullConcat ms _ = Right $ mconcat ms

fullReldee, fullReldum :: RelmapFullCons v
fullReldee = fullRelcon "reldee" reldee
fullReldum = fullRelcon "reldee" reldum

fullRelcon :: String -> Rel v -> RelmapFullCons v
fullRelcon op r _ h = Right $ RelmapConst h op r

fullCut :: (Ord v) => RelmapFullCons v
fullCut _ h = do
  let opd = halfOperand h
  term <- opd <!!> "term"
  ns   <- Syn.termNames term
  Right $ Kit.relmapCalc h "cut" (project Kit.indexCut ns)

fullEmpty :: (Ord v) => RelmapFullCons v
fullEmpty _ h = Right $ relmapEmpty h

fullJoin :: (Ord v) => RelmapFullCons v
fullJoin ms h = do
  m <- abortableHead ms
  Right $ relmapJoin h m

fullMeet :: (Ord v) => RelmapFullCons v
fullMeet ms h = do
  m <- abortableHead ms
  Right $ relmapMeet h m

fullPick :: (Ord v) => RelmapFullCons v
fullPick _ h = do
  let opd = halfOperand h
  term <- opd <!!> "term"
  ns   <- Syn.termNames term
  Right $ Kit.relmapCalc h "pick" (project Kit.indexPick ns)

fullRename :: RelmapFullCons v
fullRename _ h = do
  let opd = halfOperand h
  term <- opd <!!> "term"
  np   <- Syn.termNamePairs term
  Right $ relmapRename h np

-- | Change term names
relmapRename :: HalfRelmap -> [(String, String)] -> (Relmap v)
relmapRename h np = Kit.relmapCalc h "rename" (renameNP np)

fullSource :: RelmapFullCons v
fullSource _ h = do
  let opd = halfOperand h
  sign <- signFromOperand opd
  term <- opd <!!> "term"
  ns   <- Syn.termNames term
  Right $ relmapSource h sign ns



-- ----------------------  Utility

-- | Abortable 'head'
abortableHead :: [a] -> AbortOr a
abortableHead (x:_) = Right x
abortableHead _     = Left $ AbortLookup "head"

-- | Extract relsign from operand set
signFromOperand :: [Named [Syn.TokenTree]] -> AbortOr String
signFromOperand opd = do
  sign <- opd <!!> "sign"
  case sign of
    [Syn.Bloom (Syn.Word _ s)] -> Right s
    _ -> Left $ AbortLookup "sign"

-- ----------------------
-- $ListOfOperators
--
-- [@cut@] Project relation to unspecified terms
--
-- [@join@] Calculate join of two relations.
--
-- [@meet@] Calculate meet of two relations.
-- 
-- [@pick@] Project relation to specified terms
--
-- [@reldee@] Nullary fullset relation
--
-- [@reldum@] Nullary empty relation
--
-- [@rename@] Change term name
--
-- [@source@] Read relation from data source

