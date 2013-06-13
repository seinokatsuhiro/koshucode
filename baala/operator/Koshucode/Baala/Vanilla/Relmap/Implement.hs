{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Implement
( vanillaRelmaps
, relMaybe
) where
import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Base.Kit as Kit
import qualified Koshucode.Baala.Base.Syntax as Syn
import qualified Koshucode.Baala.Minimal as Minimal



-- ----------------------  Operators

-- | Implementation of relational operators.
vanillaRelmaps :: [Kit.RelmapImplement Val]
vanillaRelmaps = Minimal.minimalRelmaps ++ vanillaRelmapsAddition

vanillaRelmapsAddition :: [Kit.RelmapImplement Val]
vanillaRelmapsAddition = Minimal.relmaps
    -- Relmap operators in alphabetical order
    [ o "hold"   LikeHold   fullHold   ["hold EXP ..."]
    , o "maybe"  LikeMeet   fullMaybe  ["maybe RELMAP ..."]
    , o "mmaybe" LikeMeet   fullMMaybe ["mmaybe RELMAP ..."]
    , o "rdf"    LikeSource fullRdf    ["rdf SIGN /SBJ /OBJ"]
    , o "unhold" LikeHold   fullUnhold ["unhold EXP ..."]
    , o "val"    LikeVal    fullVal    ["val /NEW EXP ..."]
    ] where o = (,,,)



-- ----------------------  Constructors

fullHold :: Kit.RelmapFullCons Val
fullHold = fullHoldFor "hold" (==)

fullUnhold :: Kit.RelmapFullCons Val
fullUnhold = fullHoldFor "unhold" (/=)

fullHoldFor :: String -> (Val -> Val -> Bool) -> Kit.RelmapFullCons Val
fullHoldFor op test _ h = do
  let opd = Kit.halfOperand h
  term <- opd <!!> "term"
  Right $ Kit.relmapCalc h op (holdBody test $ Syn.Branch 0 term)

fullRdf :: Kit.RelmapFullCons Val
fullRdf _ h = do
  let opd = Kit.halfOperand h
  sign  <- Minimal.signFromOperand opd
  term  <- opd <!!> "term"
  [s,o] <- Syn.termNames term
  Right $ Kit.RelmapAlias h $
        Kit.relmapSource h sign ["/s", "/o"] `mappend`
        Minimal.relmapRename h [(s,"/s"), (o,"/o")]

fullVal :: Kit.RelmapFullCons Val
fullVal _ h = do
  let opd = Kit.halfOperand h
  term <- opd <!!> "term"
  Right $ Kit.relmapCalc h "val" (valBody term)



-- ----------------------  Maybe and MMaybe

fullMaybe :: Kit.RelmapFullCons Val
fullMaybe ms h = Right $ relmapMaybe ms h

fullMMaybe :: Kit.RelmapFullCons Val
fullMMaybe ms h = Right $ relmapMMaybe ms h

-- | like SQL's left join
relmapMaybe :: (Ord v, Nil v) =>
    [Kit.Relmap v] -> Kit.HalfRelmap -> Kit.Relmap v
relmapMaybe ms h = Kit.relmapConfl h "maybe" sub ms where
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = undefined

-- | like SQL's full join
relmapMMaybe :: (Ord v, Nil v) =>
    [Kit.Relmap v] -> Kit.HalfRelmap -> Kit.Relmap v
relmapMMaybe ms h = Kit.relmapConfl h "mmaybe" sub ms where
    sub [r2] r1 = Minimal.relJoin (relMaybe r1 r2) (relMaybe r2 r1)
    sub _ _     = undefined

-- | like SQL's left join
relMaybe :: (Ord v, Nil v) => Rel v -> Rel v -> Rel v
relMaybe r1 r2 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (headDegree h3 - headDegree h1) nil
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]
